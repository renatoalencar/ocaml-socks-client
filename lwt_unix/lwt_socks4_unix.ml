open Lwt.Infix
open Socks

let (let*) a f = a >>= f

let connect fd addr port =
  let sockaddr = Unix.(ADDR_INET (inet_addr_of_string addr, port)) in
  Lwt_io.open_connection ~fd sockaddr

let acquire command proxy_addr proxy_port  =
  let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  let* (input, output) = connect fd proxy_addr proxy_port in

  let* () =
    command
    |> Socks4.Request.to_string
    |> Lwt_io.write output
  in

  Lwt_io.read ~count:8 input
  >|= Socks4.Response.of_string
  >|= fun response ->
  match response.Socks4.Response.code with
  | `RequestGranted -> Ok (fd, Some response)
  | code -> Error (Socks4.Response.string_of_code code)

let acquire_socks4 command proxy_addr proxy_port hostname port =
  let open Lwt_result.Infix in
  let address =
    hostname
    |> Ipaddr.V4.of_string
    |> Result.map (fun ip -> `IPv4 ip)
    |> Result.map_error (function `Msg msg -> msg)
    |> Lwt_result.lift
  in
  address >>= fun address ->
  let request = Socks4.Request.make command address port in
  acquire request proxy_addr proxy_port

let acquire_socks4a command proxy_addr proxy_port hostname port =
  let addr =
    match Ipaddr.V4.of_string hostname with
    | Ok ip -> `IPv4 ip
    | Error _ -> `Domain hostname
  in
  let command = Socks4.Request.make command addr port in
  acquire command proxy_addr proxy_port

let acquire_no_proxy addr port =
  let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  connect fd addr port >|= fun _ -> Ok (fd, None)

let connect ~proxy:t hostname port =
  let response =
    match t with
    | `SOCKS4 (proxy_addr, proxy_port) -> acquire_socks4 Socks4.Request.Connect proxy_addr proxy_port hostname port
    | `SOCKS4a (proxy_addr, proxy_port) -> acquire_socks4a Socks4.Request.Connect proxy_addr proxy_port hostname port
    | `NOPROXY -> acquire_no_proxy hostname port
  in
  Lwt_result.map (fun (fd, _) -> fd) response

(* Bind is supposed to work, but is not exposed yet *)
let _bind ~proxy:t hostname port =
  let response =
    match t with
    | `SOCKS4 (proxy_addr, proxy_port) -> acquire_socks4 Socks4.Request.Bind proxy_addr proxy_port hostname port
    | `SOCKS4a (proxy_addr, proxy_port) -> acquire_socks4a Socks4.Request.Bind proxy_addr proxy_port hostname port
    | `NOPROXY -> failwith "Bind does not support NOPROXY"
  in
  Lwt_result.map (fun (fd, res) -> (fd, Option.get res)) response
