let range n =
  List.of_seq
  @@ Seq.unfold
       (fun i -> if i < n then Some (i, i + 1) else None) 0

let inet_ntoa cstruct =
  range 4
  |> List.map (Cstruct.get_byte cstruct)
  |> List.map string_of_int
  |> String.concat "."

let inet_aton addr =
  let octets =
    addr
    |> String.split_on_char '.'
    |> List.map int_of_string_opt
    |> List.filter_map (fun o -> o)
  in
  match octets with
  | a :: b :: c :: d :: [] ->
     begin
       try 
         let value = Bytes.create 4 in
         Bytes.set value 0 (Char.chr a);
         Bytes.set value 1 (Char.chr b);
         Bytes.set value 2 (Char.chr c);
         Bytes.set value 3 (Char.chr d);
         Some (Bytes.to_string value)
       with Invalid_argument from as exn ->
         match from with
         | "Char.chr" -> None
         | _ -> raise exn
     end
  | _ -> None
       
