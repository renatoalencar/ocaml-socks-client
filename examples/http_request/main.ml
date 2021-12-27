open Httpaf
open Httpaf_lwt_unix
open Lwt.Infix

let (let*) a f = a >>= f
let (let+) a f = Lwt_result.bind a f

let (>>?) = Lwt_result.bind
let (>|?) a f = Lwt_result.map f a

let response_handler on_eof _ body =
  print_endline "Received response";

  let rec on_read bs ~off ~len =
    print_endline @@ Bigstringaf.substring ~off ~len bs;
    Body.schedule_read body ~on_read ~on_eof
  in
  Body.schedule_read body ~on_read ~on_eof

let resolve_dns domain_name =
  Lwt_result.lift
    Domain_name.(Result.bind (of_string domain_name) host)
  >>? fun domain_name ->
  Dns_client_lwt.(gethostbyname (create ()) domain_name)

let resolve_to_addr socks4a hostname =
  match socks4a, Ipaddr.V4.of_string hostname with
  | _, Ok addr -> Lwt_result.return (`IPv4 addr)
  | true, Error _ -> Lwt_result.return (`Domain hostname)
  | false, Error _ -> resolve_dns hostname >|? fun addr -> `IPv4 addr

let connect_to_proxy host port =
  let happy_eyeballs = Happy_eyeballs_lwt.create () in
  Happy_eyeballs_lwt.connect happy_eyeballs host [port] >|?
  fun (_, fd) -> fd

let request fd target =
  let finished, notify_finished = Lwt.wait () in

  let error_handler error =
    let error =
      match error with
      | `Malformed_response msg -> `Msg msg
      | `Invalid_response_body_length _ -> `Msg "Invalid response body length"
      | `Exn exn -> raise exn
    in
    Lwt.wakeup_later notify_finished (Error error)
  in
  let headers = Headers.of_list [ "host", target
                                ; "accept", "*/*"
                                ; "user-agent", "httpaf" ]
  in
  let request = Request.create ~headers `GET "/" in

  let response_handler response body =
    let on_eof () = Lwt.wakeup_later notify_finished (Ok ()) in
    response_handler on_eof response body;
  in

  let body =
    Client.request
      fd
      request
      ~error_handler
      ~response_handler
  in
  Body.close_writer body;

  finished

let main socks4a socks4_host socks4_port target  =
  let* result =
    resolve_to_addr socks4a target >>? fun target_addr ->
    connect_to_proxy socks4_host socks4_port >>?
      fun fd -> begin
          Lwt_socks4_unix.connect fd target_addr 80
          |> Lwt_result.map_err (function
                 | `Eof -> `Msg "Premature end of input"
                 | `Socks4 err -> `Msg (Socks.Socks4.Response.string_of_code err))
        end >>? fun () ->
    request fd target
  in
  begin
    match result with
    | Ok () -> ()
    | Error (`Msg err) -> failwith ("Error: " ^ err)
  end;

  Lwt.return_unit

let () =
  let socks4_hostname = ref "127.0.0.1" in
  let socks4_port = ref 9050 in
  let target = ref "" in
  let socks4a = ref false in

  let speclist =
    [("-dns", Arg.Set socks4a, "Forward DNS queries to proxy (SOCKS4a)");
     ("-proxy-host", Arg.Set_string socks4_hostname, "SOCKS4 proxy hostname");
     ("-proxy-port", Arg.Set_int socks4_port, "SOCKS4 proxy port")]
  in

  let set_target host =
    target := host
  in

  Arg.parse speclist set_target
    "main.exe [-dns] [-socks4-host host] [-socks4-port port] target";

  Lwt_main.run (main !socks4a !socks4_hostname !socks4_port !target)
