open Lwt.Infix

let ( >>? ) = Lwt_result.bind
let ( <.> ) = fun f g x -> f (g x)

module Httpaf_client_connection = struct
  include Httpaf.Client_connection

  let yield_reader _ = assert false
  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield ])
end

module Main
  (Random : Mirage_random.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack: Tcpip.Stack.V4V6)
= struct
  module Happy_eyeballs = Happy_eyeballs_mirage.Make (Random) (Time) (Mclock) (Pclock) (Stack)

  module Socks4 = struct
    include Mirage_socks4.Make (Stack.TCP)

    type endpoint = Stack.TCP.flow * Socks.Socks4.Request.addr * int
    let connect (flow, addr, port) = client_of_flow flow addr port
  end

  module TLS = struct
    include Tls_mirage.Make (Socks4)

    type endpoint = Socks4.flow * [ `host ] Domain_name.t option * Tls.Config.client
    let connect (flow, host, cfg) = client_of_flow cfg ?host flow
  end

  let _socks_edn, socks_protocol = Mimic.register ~name:"socks4" (module Socks4)
  let _tls_edn, tls_protocol = Mimic.register ~name:"tls" (module TLS)

  let transmit
    : [ `read ] Httpaf.Body.t -> [ `write ] Httpaf.Body.t -> unit
    = fun src dst ->
      let rec on_eof () =
        Httpaf.Body.close_writer dst ;
        Httpaf.Body.close_reader src
      and on_read buf ~off ~len =
        Httpaf.Body.schedule_bigstring dst ~off ~len buf ;
        Httpaf.Body.schedule_read src ~on_eof ~on_read in
      Httpaf.Body.schedule_read src ~on_eof ~on_read

  let transmit_tls
    : Stack.TCP.flow -> Mimic.flow -> unit Lwt.t
    = fun flow dst ->
      let rec reader () = Stack.TCP.read flow >>= function
        | Error err ->
          Logs.err (fun m -> m "Got an error from the (reader) client: %a." Stack.TCP.pp_error err) ;
          Lwt.return_unit
        | Ok `Eof -> Lwt.return_unit
        | Ok (`Data cs) -> Mimic.write dst cs >>= function
          | Ok () -> Lwt.pause () >>= reader
          | Error err ->
            Logs.err (fun m -> m "Got an error from the (reader) destination: %a." Mimic.pp_write_error err) ;
            Lwt.return_unit in
      let rec writer () = Mimic.read dst >>= function
        | Error err ->
          Logs.err (fun m -> m "Got an error from the (writer) destination: %a." Mimic.pp_error err) ;
          Lwt.return_unit
        | Ok `Eof -> Lwt.return_unit
        | Ok (`Data cs) -> Stack.TCP.write flow cs >>= function
          | Ok () -> Lwt.pause () >>= writer
          | Error err ->
            Logs.err (fun m -> m "Got an error from the (writer) client: %a." Stack.TCP.pp_write_error err) ;
            Lwt.return_unit in
      Lwt.join [ reader (); writer () ] >>= fun () ->
      Stack.TCP.close flow >>= fun () ->
      Mimic.close dst

  let response_handler reqd resp src =
    let dst = Httpaf.Reqd.respond_with_streaming reqd resp in
    transmit src dst

  let create_connection_to_socks4 happy_eyeballs addr port =
    let module Flow = (val Mimic.repr socks_protocol) in
    Happy_eyeballs.connect happy_eyeballs
      (Key_gen.socks4_hostname ())
      [ Key_gen.socks4_port () ] >|= Rresult.R.open_error_msg >>? fun ((ipv4, _port), tcpv4v6) ->
    Socks4.client_of_flow tcpv4v6 addr port
    >|= Rresult.R.reword_error (Rresult.R.msgf "%a" Socks4.pp_write_error) >>? fun flow ->
    Lwt.return_ok (Flow.T flow)

  let create_tls_connection_to_socks4 cfg happy_eyeballs addr port =
    (* let module Flow = (val Mimic.repr tls_protocol) in *)
    let module Flow = (val Mimic.repr socks_protocol) in
    Happy_eyeballs.connect happy_eyeballs
      (Key_gen.socks4_hostname ())
      [ Key_gen.socks4_port () ] >|= Rresult.R.open_error_msg >>? fun ((ipv4, _port), tcpv4v6) ->
    Socks4.client_of_flow tcpv4v6 addr port
    >|= Rresult.R.reword_error (Rresult.R.msgf "%a" Socks4.pp_write_error) >>? fun socks4 ->
    Lwt.return_ok (Flow.T socks4)
    (* let host = match addr with
      | `IPv4 _ -> None
      | `Domain str -> Rresult.(R.to_option Domain_name.(of_string str >>= host)) in
    TLS.client_of_flow cfg ?host socks4
    >|= Rresult.R.reword_error (Rresult.R.msgf "%a" TLS.pp_write_error) >>? fun flow ->
    Lwt.return_ok (Flow.T flow) *)

  let host_to_addr host =
    let host, port = match String.split_on_char ':' host with 
      | [ host; port ] ->
        ( try host, int_of_string port
          with _ -> host, 80 )
      | _ -> host, 80 in
    let addr = match Ipaddr.V4.of_string host with
      | Ok ipaddr -> `IPv4 ipaddr
      | Error _ -> `Domain host in
    addr, port

  let error_handler = function
    | `Exn exn ->
      Logs.err (fun m -> m "Exception from the client connection: %S" (Printexc.to_string exn))
    | `Invalid_response_body_length response ->
      Logs.err (fun m -> m "Invalid response body length")
    | `Malformed_response err ->
      Logs.err (fun m -> m "Malformed response: %s" err)

  let request_handler cfg happy_eyeballs flow peer reqd =
    let request = Httpaf.Reqd.request reqd in
    let target =
      let open Rresult.R in
      Httpaf.Headers.get request.Httpaf.Request.headers "Host"
      |> of_option ~none:(fun () -> error `Host_not_found)
      >>| host_to_addr in
    match target with
    | Error `Host_not_found ->
      Logs.err (fun m -> m "The request missed the Host parameter.")
    | Ok (addr, port) ->
      Lwt.async begin fun () -> 
      Lwt.catch begin fun () -> match request.Httpaf.Request.meth with
      | `CONNECT ->
        ( create_tls_connection_to_socks4 cfg happy_eyeballs addr port >>= function
        | Ok dst ->
          let headers = Httpaf.Headers.of_list [ "connection", "close" ] in
          let resp = Httpaf.Response.create ~headers `OK in
          Httpaf.Reqd.respond_with_string reqd resp "" ;
          Httpaf.Body.close_reader (Httpaf.Reqd.request_body reqd) ;
          transmit_tls flow dst
        | Error _ -> assert false )
      | _meth ->
        let dst, conn = Httpaf.Client_connection.request
          ~error_handler:error_handler
          ~response_handler:(response_handler reqd) request in
        transmit (Httpaf.Reqd.request_body reqd) dst ;
        create_connection_to_socks4 happy_eyeballs addr port >>= function
        | Ok dst ->
          Paf.run (module Httpaf_client_connection) ~sleep:Time.sleep_ns conn dst >>= fun () ->
          Stack.TCP.close flow
        | Error (`Msg err) ->
          Logs.err (fun m -> m "Impossible to create a client connection: %s." err) ;
          Lwt.return_unit
      end @@ fun exn ->
      Logs.err (fun m -> m "Got an exception while sending the HTTP request: %S" (Printexc.to_string exn)) ;
      Lwt.return_unit
      end

  module Stack' = struct
    module TCP = struct
      include Stack.TCP

      let close _ = Lwt.return_unit
    end

    module UDP = Stack.UDP
    module IP = Stack.IP

    type t = Stack.t
    let disconnect = Stack.disconnect
    let udp = Stack.udp
    let tcp = Stack.tcp
    let ip = Stack.ip
    let listen_udp = Stack.listen_udp
    let listen_tcp = Stack.listen_tcp
    let listen = Stack.listen
  end

  module Paf = Paf_mirage.Make (Time) (Stack')

  let of_fp str =
    let hash, fp =
      let hash_of_string = function
        | "md5" -> Some `MD5
        | "sha" | "sha1" -> Some `SHA1
        | "sha224" -> Some `SHA224
        | "sha256" -> Some `SHA256
        | "sha384" -> Some `SHA384
        | "sha512" -> Some `SHA512
        | _ -> None
      in
      match String.split_on_char ':' str with
      | [] -> Fmt.failwith "Invalid fingerprint %S" str
      | [ fp ] -> `SHA256, fp
      | hash :: rest -> (
          match hash_of_string (String.lowercase_ascii hash) with
          | Some hash -> hash, String.concat "" rest
          | None -> Fmt.failwith "Invalid hash algorithm: %S" hash)
    in
    let fp =
      try Hex.to_cstruct (`Hex fp)
      with _ -> Fmt.failwith "Invalid hex fingerprint value: %S" fp
    in
    hash, fp

  let authenticator () =
    let time () = Some (Ptime.v (Pclock.now_d_ps ())) in
    match Key_gen.tls_key_fingerprint (), Key_gen.tls_cert_fingerprint () with
    | None, None ->
      let module NSS = Ca_certs_nss.Make (Pclock) in
      ( match NSS.authenticator () with
      | Ok authenticator -> authenticator
      | Error (`Msg err) -> failwith err )
    | Some _, Some _ ->
      failwith "You can not provide certificate and key fingerprint"
    | Some fp, None ->
      let hash, fingerprint = of_fp fp in
      X509.Authenticator.server_key_fingerprint ~time ~hash ~fingerprint
    | None, Some fp ->
      let hash, fingerprint = of_fp fp in
      X509.Authenticator.server_cert_fingerprint ~time ~hash ~fingerprint

  let error_handler _peer ?request _err _respond = ()

  let start _random _time _mclock _pclock stackv4v6 =
    let happy_eyeballs = Happy_eyeballs.create stackv4v6 in
    let cfg = Tls.Config.client ~authenticator:(authenticator ()) () in
    Paf.init ~port:8080 (Stack.tcp stackv4v6) >>= fun t ->
    let service = Paf.http_service ~error_handler
      (request_handler cfg happy_eyeballs) in
    let `Initialized th = Paf.serve service t in th
end
