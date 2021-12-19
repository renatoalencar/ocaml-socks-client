open Socks
open Lwt.Infix

module Make (S: Tcpip.Stack.V4) = struct
  type t = { tcpv4: S.TCPV4.t
           ; proxy: Ipaddr.V4.t * int }

  type error =
    [ `Connection of S.TCPV4.error
    | `Read of S.TCPV4.error
    | `Eof
    | `Write of S.TCPV4.write_error
    | `Socks4 of Socks4.Response.code ]

  let make stack proxy =
    { tcpv4 = S.tcpv4 stack
    ; proxy }

  let create_connection ipv4 proxy_ip proxy_port =
    S.TCPV4.create_connection ipv4 (proxy_ip, proxy_port) >|= function
    | Ok flow -> Ok flow
    | Error err -> Error (`Connection err)

  let send_connection_request flow host port =
    let buf = Socks4.Request.(to_cstruct (connect host port)) in
    Lwt_result.map_err
      (fun err -> `Write err)
      (S.TCPV4.write flow buf)

  let read count flow =
    let rec aux read bufs =
      if read < count then
        S.TCPV4.read flow >>= function
        | Ok (`Data buf) -> aux (read + Cstruct.length buf) (buf :: bufs)
        | Ok `Eof -> Lwt_result.fail `Eof
        | Error error -> Lwt_result.fail (`Read error)
      else
        Lwt_result.return bufs
    in
    Lwt_result.map
      (fun bufs -> Cstruct.(sub (concat (List.rev bufs)) 0 count))
      (aux 0 [])

  let read_response flow =
    Lwt_result.map Socks4.Response.of_cstruct (read 8 flow)

  let connect t host port =
    let open Lwt_result.Infix in
    let (proxy_ip, proxy_port) = t.proxy in

    create_connection t.tcpv4 proxy_ip proxy_port >>= fun flow ->
    send_connection_request flow host port >>= fun _ ->
    read_response flow >>= fun response ->
    match response.Socks4.Response.code with
    | `RequestGranted -> Lwt_result.return flow
    | error -> Lwt_result.fail (`Socks4 error)
end
