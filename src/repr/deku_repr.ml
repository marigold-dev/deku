module Prefix = struct
  type prefix = string
  type t = prefix

  include Base58.Prefix
end

(* TODO: exceptions???*)
exception Not_a_string
exception Not_a_b58

let unexpected_data ~name =
  Format.kasprintf invalid_arg "Unexpected data (%s)" name

module With_yojson_of_b58 (P : sig
  type t

  val of_b58 : string -> t option
  val to_b58 : t -> string
end) =
struct
  open P

  let t_of_yojson json =
    match json with
    | `String string -> (
        match of_b58 string with Some t -> t | None -> raise Not_a_b58)
    | _ -> raise Not_a_string

  let yojson_of_t t = `String (to_b58 t)
end

module With_encoding_of_b58 (P : sig
  type t

  val name : string
  val title : string
  val raw_encoding : t Data_encoding.t
  val of_b58 : string -> t option
  val to_b58 : t -> string
end) =
struct
  open Data_encoding
  open P

  let encoding =
    let of_b58_exn string =
      match of_b58 string with Some t -> t | None -> unexpected_data ~name
    in
    let json_encoding = conv to_b58 (Json.wrap_error of_b58_exn) string in
    splitted
      ~binary:(obj1 (req name raw_encoding))
      ~json:(def name ~title:(title ^ " (Base58Check-encoded)") json_encoding)
end

module With_encodings (P : sig
  type t

  val prefix : Prefix.t
  val to_raw : t -> string
  val of_raw : string -> t option
end) =
struct
  open P

  let of_b58 string = Base58.simple_decode ~prefix ~of_raw string
  let to_b58 t = Base58.simple_encode ~prefix ~to_raw t

  include With_yojson_of_b58 (struct
    type t = P.t

    let of_b58 = of_b58
    let to_b58 = to_b58
  end)
end

module With_all_encodings (P : sig
  type t

  val name : string
  val title : string
  val prefix : Prefix.t
  val size : int
  val to_raw : t -> string
  val of_raw : string -> t option
end) =
struct
  open P
  include With_encodings (P)

  include With_encoding_of_b58 (struct
    type t = P.t

    let name = P.name
    let title = P.title

    let of_raw_exn string =
      match of_raw string with Some t -> t | None -> unexpected_data ~name

    let raw_encoding =
      let open Data_encoding in
      conv to_raw of_raw_exn (Fixed.string size)

    let of_b58 = of_b58
    let to_b58 = to_b58
  end)
end

(* TODO: this is dumb *)
let rec decode_variant l string =
  match l with
  | of_b58 :: l -> (
      match of_b58 string with
      | Some v -> Some v
      | None -> decode_variant l string)
  | [] -> None

module With_encodings_of_many (P : sig
  type t

  val of_b58 : (string -> t option) list
  val to_b58 : t -> string
end) =
struct
  open P

  let of_b58 string = decode_variant of_b58 string
  let to_b58 = to_b58

  include With_yojson_of_b58 (struct
    type t = P.t

    let of_b58 = of_b58
    let to_b58 = to_b58
  end)
end

module With_all_encodings_of_many (P : sig
  type t

  val key : string
  val name : string
  val title : string
  val cases : t Data_encoding.case list
  val of_b58 : (string -> t option) list
  val to_b58 : t -> string
end) =
struct
  open P
  include With_encodings_of_many (P)

  include With_encoding_of_b58 (struct
    type t = P.t

    let name = name
    let title = title

    let raw_encoding =
      let open Data_encoding in
      def key ~description:title @@ union cases

    let of_b58 = of_b58
    let to_b58 = to_b58
  end)
end
