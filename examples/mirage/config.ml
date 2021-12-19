open Mirage

let packages = [ package "socks" ]

let main = foreign ~packages "Unikernel.Main" (stackv4 @-> job)

let stack = generic_stackv4 default_network

let () =
  register "example" [
      main $ stack
    ]
