open Socks.Socks4

type error = [ `Eof | `Socks4 of Socks.Socks4.Response.code ]

val connect :
  Lwt_unix.file_descr ->
  Request.addr ->
  int ->
  (unit, error) Lwt_result.t
(** [connect fd hostname port] establishes a connection to [hostname]
    on [port] through a proxy that should be already connected on [fd]. *)
