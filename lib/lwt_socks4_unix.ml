open Lwt.Infix

let (let*) a f = a >>= f

let validate_ip_address ip =
  let octets = String.split_on_char '.' ip in
  List.length octets = 8
  && (List.for_all ((>) 256) @@ List.map int_of_string octets)

let connect fd sockaddr =
  Lwt_io.open_connection ~fd sockaddr

let resolve_addr hostname port =
  let rec find_addr_inet (info : Unix.addr_info list) =
    match info with
    | [] -> failwith "Address not found"
    | { ai_addr = Unix.ADDR_INET (addr, _) ; _ } :: _ ->
       Unix.ADDR_INET (addr, port)
    | rest -> find_addr_inet rest
  in
  find_addr_inet (Unix.(getaddrinfo hostname "" [ AI_FAMILY PF_INET ]))

let resolve_ip_addr hostname port =
  match resolve_addr hostname port with
  | Unix.ADDR_INET (inet, _) -> Unix.string_of_inet_addr inet
  | _ -> assert false

let acquire proxy_addr proxy_port addr port =
  let sockaddr = Unix.(ADDR_INET (inet_addr_of_string proxy_addr, proxy_port)) in
  let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in

  let* (input, output) = connect fd sockaddr in

  let* () =
    Socks4.Request.connect addr port
    |> Socks4.Request.to_string
    |> Lwt_io.write output
  in

  Lwt_io.read ~count:8 input
  >|= Socks4.Response.of_string
  >|= fun response ->
  match response.Socks4.Response.code with
  | `RequestGranted -> Ok fd
  | code -> Error (Socks4.Response.string_of_code code)

let acquire_socks4 proxy hostname port =
  let hostname = 
    if validate_ip_address hostname then
      hostname
    else
      resolve_ip_addr hostname port
  in

  let (proxy_addr, proxy_port) =
    match proxy with
    | `SOCKS4 proxy -> proxy
    | _ -> assert false
  in
  acquire proxy_addr proxy_port hostname port

let acquire_socks4a proxy hostname port =
  let (proxy_addr, proxy_port) =
    match proxy with
    | `SOCKS4a proxy -> proxy
    | _ -> assert false
  in
  acquire proxy_addr proxy_port hostname port

let acquire_no_proxy hostname port =
  let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  let addr = resolve_addr hostname port in
  connect fd addr >|= fun _ -> Ok fd

let acquire t hostname port =
  match t with
  | `SOCKS4 _ as proxy -> acquire_socks4 proxy hostname port
  | `SOCKS4a _ as proxy -> acquire_socks4a proxy hostname port
  | `NOPROXY -> acquire_no_proxy hostname port
