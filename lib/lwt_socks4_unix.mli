
val acquire :
  Socks4.proxy ->
  string ->
  int ->
  (Lwt_unix.file_descr, string) Lwt_result.t
(** [acquire proxy hostname port] establishes a connection the
    proxy and initializes by sending and validating the response. *)

