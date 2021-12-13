[%%cstruct
type header = {
  version: uint8_t;
  command: uint8_t;
  port: uint16_t;
  ip: uint8_t [@len 4];
} [@@big_endian]]

type proxy = [ `SOCKS4 of (string * int)
             | `SOCKS4a of (string * int)
             | `NOPROXY ]

module Request = struct
  type command = Connect | Bind

  type t = { command: command
           ; addr: string
           ; port: int }

  let make command addr port =
    { command
    ; addr
    ; port }

  let connect addr port =
    make Connect addr port

  let bind addr port =
    make Bind addr port

  let command_to_int = function
  | Connect -> 1
  | Bind -> 2

  (* An IP address used for SOCKS4a requests *)
  let socks4a_ip = "\000\000\000\001"

  let to_cstruct req =
    let (ipaddress, is_domain_name) =
      match Socket.inet_aton req.addr with
      | Some ip -> (ip, false)
      | None -> (socks4a_ip, true)
    in
    let buf =
      let bufsize =
        9 + if is_domain_name then
              String.length req.addr + 1
            else 0
      in
      Cstruct.create bufsize
    in
    set_header_version buf 4;
    set_header_command buf (command_to_int req.command);
    set_header_port buf req.port;
    set_header_ip ipaddress 0 buf;

    if is_domain_name then
      Cstruct.blit_from_string req.addr 0 buf 9 (String.length req.addr);

    buf

  let to_string req =
    Cstruct.to_string @@ to_cstruct req

end

module Response = struct
  type code =
    [ `RequestGranted
    | `RequestFailed
    | `RequestRejectedIdentd
    | `UserIdNotMatching ]
 
  type t = { code: code
           ; ip: string
           ; port: int }

  let code_of_int = function
    | 90 -> `RequestGranted
    | 91 -> `RequestFailed
    | 92 -> `RequestRejectedIdentd
    | 93 -> `UserIdNotMatching
    | _  -> raise (Invalid_argument "Response.code_of_int")

  let string_of_code = function
    | `RequestGranted -> "RequestGranted"
    | `RequestFailed -> "RequestFailed"
    | `RequestRejectedIdentd -> "RequestRejectedIdentd"
    | `UserIdNotMatching -> "UserIdNotMatching"

  let of_cstruct cstruct =
    let () =
      match get_header_version cstruct with
      | 0 -> ()
      | _ -> raise (Invalid_argument "Response.of_cstruct")
    in
    { code = code_of_int (get_header_command cstruct)
    ; ip = Socket.inet_ntoa (get_header_ip cstruct)
    ; port = get_header_port cstruct }

  let of_string str =
    of_cstruct @@ Cstruct.of_string str

end
