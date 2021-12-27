open Mirage

let packages = [ package "socks"
               ; package "mirage_socks4"
               ; package "paf"
               ; package "paf" ~sublibs:[ "mirage" ]
               ; package "httpaf"
               ; package "rresult"
               ; package "hex"
               ; package "happy-eyeballs-mirage" ]

let socks4_hostname =
  let doc = Key.Arg.info ~doc:"SOCKS4 hostname." [ "socks4-hostname" ] in
  Key.(create "socks4_hostname" Arg.(required string doc))

let socks4_port =
  let doc = Key.Arg.info ~doc:"SOCKS4 port." [ "socks4-port" ] in
  Key.(create "socks4_port" Arg.(opt int 1080 doc))

let port =
  let doc = Key.Arg.info ~doc:"Port of the HTTP proxy." [ "port" ] in
  Key.(create "port" Arg.(opt int 8080 doc))

let main = foreign
  ~keys:[ Key.abstract socks4_hostname
        ; Key.abstract socks4_port
        ; Key.abstract port ]
  ~packages "Unikernel.Main"
  (random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> job)

let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let stackv4v6 = generic_stackv4v6 default_network

let () =
  register "example"
    [ main $ random $ time $ mclock $ pclock $ stackv4v6 ]
