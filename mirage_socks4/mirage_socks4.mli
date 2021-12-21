module Make (Flow: Mirage_flow.S) : sig
  include Mirage_flow.S

  val client_of_flow : Flow.flow -> Socks.Socks4.Request.addr -> int -> (flow, write_error) result Lwt.t
end
