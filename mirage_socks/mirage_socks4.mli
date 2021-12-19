
module Make (S: Mirage_stack.V4) : sig
  type t

  type error =
    [ `Connection of S.TCPV4.error
    | `Read of S.TCPV4.error
    | `Eof
    | `Write of S.TCPV4.write_error
    | `Socks4 of Socks.Socks4.Response.code ]

  val make : S.t -> Ipaddr.V4.t * int -> t

  val connect : t -> Socks.Socks4.Request.addr -> int -> (S.TCPV4.flow, error) Lwt_result.t
end
