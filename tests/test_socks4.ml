open Socks

let test_socks4_connect_ip_address () =
  let request = Socks4.Request.connect (`IPv4 (Ipaddr.V4.of_string_exn "192.168.0.1")) 80 in
  let buf = Socks4.Request.to_string request in
  Alcotest.(check string) "Request should match"
    buf "\004\001\000\080\192\168\000\001\000"

let test_socks4_bind_ip_address () =
  let request = Socks4.Request.bind  (`IPv4 (Ipaddr.V4.of_string_exn "192.168.0.1")) 80 in
  let buf = Socks4.Request.to_string request in
  Alcotest.(check string) "Request should match"
    buf "\004\002\000\080\192\168\000\001\000"

let test_socks4a_connect_domain_name () =
  let request = Socks4.Request.connect (`Domain "ocaml.org") 80 in
  let buf = Socks4.Request.to_string request in
  Alcotest.(check string) "Request should match"
    buf "\004\001\000\080\000\000\000\001\000ocaml.org\000"

let test_socks4a_bind_domain_name () =
  let request = Socks4.Request.bind (`Domain "ocaml.org") 80 in
  let buf = Socks4.Request.to_string request in
  Alcotest.(check string) "Request should match"
    buf "\004\002\000\080\000\000\000\001\000ocaml.org\000"

let code =
  let format_code = function
    | `RequestGranted -> "RequestGranted"
    | `RequestFailed -> "RequestFailed"
    | `RequestRejectedIdentd -> "RequestRejectedIdentd"
    | `UserIdNotMatching -> "UserIdNotMatching"
  in
  let compare a b =
    match a, b with
      | (`RequestGranted, `RequestGranted)
      | (`RequestFailed, `RequestFailed)
      | (`RequestRejectedIdentd, `RequestRejectedIdentd)
      | (`UserIdNotMatching, `UserIdNotMatching) -> true
      | _ -> false
  in
  let pp_code ppf x = Fmt.pf ppf "%s" (format_code x) in
  Alcotest.testable pp_code compare

let test_socks4_response_success () =
  let response = Socks4.Response.of_string "\000\090\000\080\192\168\000\001" in
  Alcotest.(check string) "Parse IP address" (Ipaddr.V4.to_string response.ip) "192.168.0.1";
  Alcotest.(check int) "Parse port" response.port 80;
  Alcotest.(check code) "Parse code" response.code `RequestGranted

let test_socks4_response_failed () =
  let response = Cstruct.of_string "\000\091\000\080\192\168\000\001" in
  let response = Socks4.Response.of_cstruct response in
  Alcotest.(check string) "Parse IP address" (Ipaddr.V4.to_string response.ip) "192.168.0.1";
  Alcotest.(check int) "Parse port" response.port 80;
  Alcotest.(check code) "Parse code" response.code `RequestFailed

let test_socks4_response_identd () =
  let response = Cstruct.of_string "\000\092\000\080\192\168\000\001" in
  let response = Socks4.Response.of_cstruct response in
  Alcotest.(check string) "Parse IP address" (Ipaddr.V4.to_string response.ip) "192.168.0.1";
  Alcotest.(check int) "Parse port" response.port 80;
  Alcotest.(check code) "Parse code" response.code `RequestRejectedIdentd

let test_socks4_response_userid () =
  let response = Cstruct.of_string "\000\093\000\080\192\168\000\001" in
  let response = Socks4.Response.of_cstruct response in
  Alcotest.(check string) "Parse IP address" (Ipaddr.V4.to_string response.ip) "192.168.0.1";
  Alcotest.(check int) "Parse port" response.port 80;
  Alcotest.(check code) "Parse code" response.code `UserIdNotMatching

let () =
  let open Alcotest in
  run "Socks4" [
      "Request", [ test_case "SOCKS4 connect request"
                     `Quick test_socks4_connect_ip_address
                 ; test_case "SOCKS4 bind request"
                     `Quick test_socks4_bind_ip_address
                 ; test_case "SOCKS4a connect with domain name"
                     `Quick test_socks4a_connect_domain_name
                 ; test_case "SOCKS4a bind request with domain name"
                     `Quick test_socks4a_bind_domain_name ];
      "Response", [ test_case "Request granted"
                      `Quick test_socks4_response_success
                  ; test_case "Request rejected"
                      `Quick test_socks4_response_failed
                  ; test_case "Request rejected identd"
                      `Quick test_socks4_response_identd
                  ; test_case "User ID not matching"
                      `Quick test_socks4_response_userid ]
    ]
