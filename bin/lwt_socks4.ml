let make_http_request hostname =
  let lines = [ "GET / HTTP/1.1"
              ; "Host: " ^ hostname
              ; "User-Agent: curl/7.79.1"
              ; "Accept: */*"
              ; "Connection: close"
              ; ""
              ; "" (* whyyyyy? *)] in
  String.concat "\r\n" lines

open Lwt.Infix
let (let*) a f = Lwt.bind a f

module Proxy = struct
  type t =
    | Socks4 of (string * int)
    | Socks4a of (string * int)
    | NoProxy

  let socks4 hostname port =
    Socks4 (hostname, port)

  let socks4a hostname port =
    Socks4a (hostname, port)

  let no_proxy = NoProxy

  let validate_ip_address ip =
    let octets = String.split_on_char '.' ip in
    List.length octets = 8
    && (List.for_all ((>) 256) @@ List.map int_of_string octets)

  let socks4_code_to_string = function
    | `RequestGranted -> "RequestGranted"
    | `RequestFailed -> "RequestFailed"
    | `RequestRejectedIdentd -> "RequestRejectedIdentd"
    | `UserIdNotMatching -> "UserIdNotMatching"
    
  let acquire proxy_addr proxy_port addr port =
    let sockaddr = Unix.(ADDR_INET (inet_addr_of_string proxy_addr, proxy_port)) in
    let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in

    let* () =
      let* (input, output) = Lwt_io.open_connection ~fd sockaddr in

      let* () =
        Socks4.Request.connect addr port
        |> Socks4.Request.to_string
        |> Lwt_io.write output
      in

      let* response =
        Lwt_io.read ~count:8 input
        >|= Socks4.Response.of_string
      in
      match response.Socks4.Response.code with
      | `RequestGranted -> Lwt.return_unit
      | code -> failwith (socks4_code_to_string code)
    in
    Lwt.return fd

  let acquire_socks4 proxy hostname port =
    let hostname = 
      if validate_ip_address hostname then
        hostname
      else
        (* TODO: Resolve hostname to IP address,
           client should explicitly request for SOCKS4a
           and to resolve the hostname on the proxy. *)
        assert false
    in

    let (proxy_addr, proxy_port) =
      match proxy with
      | Socks4 proxy -> proxy
      | _ -> assert false
    in
    acquire proxy_addr proxy_port hostname port

  let acquire_socks4a proxy hostname port =
    let (proxy_addr, proxy_port) =
      match proxy with
      | Socks4a proxy -> proxy
      | _ -> assert false
    in
    acquire proxy_addr proxy_port hostname port

  let acquire_no_proxy hostname port =
    let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    let* () =
      let addr =
        let info = Unix.(getaddrinfo hostname "" [ AI_FAMILY PF_INET ]) in
        (* TODO: Iter through all the list *)
        match info with
        | { ai_addr = Unix.ADDR_INET (addr, _) ; _ } :: _ ->
           Unix.ADDR_INET (addr, port)
        | _ -> assert false
      in
      Lwt_unix.connect fd addr
    in
    Lwt.return fd

  let acquire t hostname port =
    match t with
    | Socks4 _ as proxy ->
       acquire_socks4 proxy hostname port
    | Socks4a _ as proxy ->
       acquire_socks4a proxy hostname port
    | NoProxy ->
       acquire_no_proxy hostname port
end

let connection_handler hostname (input, output) =
  let* () =
    print_endline "Sending request";
    let req = make_http_request hostname in
    print_endline req;
    Lwt_io.write output req
  in

  print_endline "Receiving response";
  let* response = Lwt_io.read input in

  print_endline "Received response";
  Lwt.return @@ print_endline response

let domain_name name =
  let speak_ok_or_die result = (* https://youtu.be/Cuz3t3eUqVs *)
    match result with
    | Ok value -> value
    | Error (`Msg error) -> failwith error
  in
  name
  |> Domain_name.of_string
  |> speak_ok_or_die
  |> Domain_name.host
  |> speak_ok_or_die

let main hostname =
  let target = hostname in
  let port = 443 in

  let* pair =
    let authenticator =
      match Ca_certs.authenticator () with
      | Ok auth -> auth
      | Error (`Msg msg) -> failwith msg
    in

    print_endline "Acquiring proxy";
    let proxy = Proxy.socks4a "127.0.0.1" 9050 in
    let* fd = Proxy.acquire proxy target port in

    let* client =
      print_endline "Connecting";
      let host = domain_name target in
      Tls_lwt.Unix.client_of_fd
        (Tls.Config.client ~authenticator ())
        ~host
        fd
    in
    print_endline "Connected...";
    Lwt.return (Tls_lwt.of_t client)
  in
  (connection_handler target pair)

let () =
  Lwt_main.run (main Sys.argv.(1))
