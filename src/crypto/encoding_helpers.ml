let unexpected_data ~name =
  Format.kasprintf invalid_arg "Unexpected data (%s)" name
let make_encoding ~name ~title ~to_string ~of_string ~raw_encoding =
  let open Data_encoding in
  let of_string_exn string =
    match of_string string with
    | Some t -> t
    | None -> unexpected_data ~name in
  let json_encoding = conv to_string (Json.wrap_error of_string_exn) string in
  splitted
    ~binary:(obj1 (req name raw_encoding))
    ~json:(def name ~title:(title ^ " (Base58Check-encoded)") json_encoding)
let rec parse_string_variant l string =
  match l with
  | of_string :: l -> (
    match of_string string with
    | Some v -> Some v
    | None -> parse_string_variant l string)
  | [] -> None
module Make_b58 (H : sig
  type t
  val name : string
  val title : string
  val prefix : string
  val size : int
  val to_raw : t -> string
  val of_raw : string -> t option
end) =
struct
  open H
  let size = size
  let to_raw = H.to_raw

  let to_string t = Base58.simple_encode ~prefix ~to_raw t
  let of_string string = Base58.simple_decode ~prefix ~of_raw string
  let of_raw_exn string =
    match of_raw string with
    | Some t -> t
    | None -> unexpected_data ~name
  let encoding =
    make_encoding ~name ~title ~to_string ~of_string
      ~raw_encoding:
        (let open Data_encoding in
        conv to_raw of_raw_exn (Fixed.string size))
end
