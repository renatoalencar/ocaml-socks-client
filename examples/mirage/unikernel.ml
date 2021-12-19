open Lwt.Infix

let (let*) a f = a >>= f

module Main (S: Mirage_stack.V4) = struct
  module Proxy = Mirage_socks4.Make(S)

  let make_request host =
    [ "GET / HTTP/1.1"
    ; "Host: " ^ host
    ; ""
    ; "" ]
    |> String.concat "\r\n"
    |> Cstruct.of_string

  let send flow buf =
    S.TCPV4.write flow buf >|=
    Result.get_ok

  let rec read_data flow =
    S.TCPV4.read flow >>= function
    | Ok (`Data b) ->
       Logs.info (fun f -> f "read: %d bytes:\n%s" (Cstruct.length b) (Cstruct.to_string b));
       read_data flow
    | Ok `Eof -> S.TCPV4.close flow

  let start s =
    let proxy_addr = (Ipaddr.V4.of_string_exn "127.0.0.1", 9050) in
    let proxy = Proxy.make s proxy_addr in

    let hostname = "torchdeedp3i2jigzjdmfpn5ttjhthh5wbmda2rr3jvqjg5p77c54dqd.onion" in
    let* flow =
      Proxy.connect proxy (`Domain hostname) 80 >|= function
      | Ok flow -> flow
      | Error _ -> failwith "Something went wrong"
    in
    let buf = make_request hostname in
    let* () = send flow buf in
    read_data flow

end
