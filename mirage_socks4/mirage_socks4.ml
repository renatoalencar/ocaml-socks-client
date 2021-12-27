open Socks
open Lwt.Infix

let ( >>? ) = Lwt_result.bind

module Make (Flow: Mirage_flow.S) = struct
  type flow = { flow : Flow.flow
              ; proxy : Socks4.Request.addr * int }

  type error =
    [ `Read of Flow.error
    | `Write of Flow.write_error
    | `Socks4 of Socks4.Response.code ]

  type write_error = [ Mirage_flow.write_error | error ]

  let pp_error ppf = function
    | `Read err -> Flow.pp_error ppf err
    | `Write err -> Flow.pp_write_error ppf err
    | `Socks4 code -> Fmt.pf ppf "Unexpected SOCKS4 code response: %a" Socks4.Response.pp code

  let pp_write_error ppf = function
    | #error as err -> pp_error ppf err
    | #Mirage_flow.write_error as err -> Mirage_flow.pp_write_error ppf err

  let read count flow =
    let rec aux read bufs =
      if read < count then
        Flow.read flow >>= function
        | Ok (`Data buf) -> aux (read + Cstruct.length buf) (buf :: bufs)
        | Ok `Eof -> Lwt_result.fail `Closed
        | Error error -> Lwt_result.fail (`Read error)
      else
        Lwt_result.return bufs
    in
    Lwt_result.map
      (fun bufs -> Cstruct.(sub (concat (List.rev bufs)) 0 count))
      (aux 0 [])

  let read_response flow =
    Lwt_result.map Socks4.Response.of_cstruct (read 8 flow)

  let send_connection_request flow host port =
    let buf = Socks4.Request.(to_cstruct (connect host port)) in
    Lwt_result.map_err
      (fun err -> `Write err)
      (Flow.write flow buf)

  let client_of_flow flow host port =
    send_connection_request flow host port >>? fun () ->
    read_response flow >>? fun response ->
    match response.Socks4.Response.code with
    | `RequestGranted -> Lwt_result.return { flow; proxy= host, port; }
    | error -> Lwt_result.fail (`Socks4 error)

  let read { flow; _ } =
    Lwt_result.map_err (fun err -> `Read err) (Flow.read flow)
  let write { flow; _ } buf =
    Lwt_result.map_err (fun err -> `Write err) (Flow.write flow buf)
  let writev { flow; _ } buf =
    Lwt_result.map_err (fun err -> `Write err) (Flow.writev flow buf)
  let close { flow; _ } = Flow.close flow
end
