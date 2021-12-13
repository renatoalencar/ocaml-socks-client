(** Higher level SOCKS4 protocol primitives and un/serialization *)

type proxy = [ `SOCKS4 of (string * int)
             | `SOCKS4a of (string * int)
             | `NOPROXY ]
(** Which proxy to use

    For [SOCKS4], IP address is resolved from the current host or OS
    and the IP used for the connection with the proxy. It follows the
    initial specs of the SOCKS4 protocol.

    * https://www.openssh.com/txt/socks4.protocol

    For [SOCKS4a], domain name is sent with the request to be resolved
    by the proxy itself. Observe Tor addresses (`.to' domains) should
    use this instead of SOCKS4 because Tor itself needs to resolve the
    hidden service from the address.

    * https://www.openssh.com/txt/socks4a.protocol

    [NOPROXY] allows to disable proxy at all, the host name is resolved
    and connection is established by the proxy client implementation.
*)
    
(** Defines a [Request] operation packet

    There are two operation types are defined: [Connect] and [Bind].
*)
module Request : sig
  type t

  type command = Connect | Bind
  (** Which operation request to the server *)

  val make : command -> string -> int -> t
  (** [make command hostname port] creates a request of operation [command]
      to [hostname] on [port]. *)

  val connect : string -> int -> t
  (** [connect hostname port] creates a [Connect] request to [hostname] on
      [port], which establishes a connection to the proxy and after initializing
      it forwards data between the two peers. *)

  val bind : string -> int -> t
  (** [bind hostname port] creates a [Bind] request that listens to connections
      from [hostname].

      [Bind] exists in order to support such sequences where the client after
      making a connection needs to establish an inbound connection from the
      application server. It should be used after a previous [Connect] operation
      has been made, the [hostname] and [port] parameters must be the same
      used before to the [Connect] request. The proxy should return a response
      with an IP address and a port number that it is accepting connections.
      Since the proxy may be multi-host, the IP address may not be the same
      as used to establish the connection. *)

  val to_cstruct : t -> Cstruct.t
  (** [to_cstruct request] serializes a request as [Cstruct.t] buffer. *)

  val to_string : t -> string
  (** [to_string request] serializes a request as a [string]. *)
end

(** Definition of the [Response] from the proxy server. *)
module Response : sig
  type code =
    [ `RequestGranted
    | `RequestFailed
    | `RequestRejectedIdentd
    | `UserIdNotMatching ]
  (** The response code from the request previously submitted. *)

  type t = { code: code
           ; ip: string
           ; port: int }
  (** Actual response structure.

      When using bind, [ip] and [port] contains the address and port
      that the proxy is accepting connections to be forwarded to the
      client. *)

  val string_of_code : code -> string
  (** [string_of_code code] converts a [code] into a [string], mainly to be
      used for error reporting. *)

  val of_cstruct : Cstruct.t -> t
  (** [of_cstruct buffer] converts a [Cstruct.t] buffer into a [Response.t]. *)

  val of_string : string -> t
  (** [of_string buffer] converts a [string] into a [Response.t]. *)
end
