open Lwt.Infix
open Socks

type error = [ `Eof | `Socks4 of Socks4.Response.code ]

let send_connection_request output addr port =
  let req = Socks4.Request.(connect addr port |> to_string) in
  Lwt_io.write output req

let receive_response input =
  Lwt_io.read ~count:8 input >|= fun response ->
  if String.length response < 8 then
    Error `Eof
  else
    Ok (Socks4.Response.of_string response)

let connect fd addr port =
  let input, output =
    Lwt_io.(of_fd ~mode:Input fd),
    Lwt_io.(of_fd ~mode:Output fd)
  in
  send_connection_request output addr port >>= fun () ->
  receive_response input >|= fun response ->
  Result.bind response begin fun response ->
    match response.Socks4.Response.code with
    | `RequestGranted -> Ok ()
    | error -> Error (`Socks4 error)
    end
