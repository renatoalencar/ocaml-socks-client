open Lwt.Infix

let (let*) a f = a >>= f

let make_http_request hostname =
  let lines = [ "GET / HTTP/1.1"
              ; "Host: " ^ hostname
              ; "User-Agent: curl/7.79.1"
              ; "Accept: */*"
              ; "Connection: close"
              ; ""
              ; "" (* whyyyyy? *)] in
  String.concat "\r\n" lines

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
  name
  |> Domain_name.of_string
  |> Result.get_ok
  |> Domain_name.host
  |> Result.get_ok

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
    let proxy = `SOCKS4a ("127.0.0.1", 9050) in
    let* fd =
      Lwt_socks4_unix.connect ~proxy target port >|= fun result ->
      match result with
      | Ok fd -> fd
      | Error msg -> failwith msg
    in

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
