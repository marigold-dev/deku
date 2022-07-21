module Prefix = Base58.Prefix

module With_b58 (P : sig
  type t

  val prefix : string
  val to_raw : t -> string
  val of_raw : string -> t option
end) =
struct
  open P

  let of_b58 string = Base58.simple_decode ~prefix ~of_raw string
  let to_b58 t = Base58.simple_encode ~prefix ~to_raw t
end

(* TODO: exceptions???*)
exception Not_a_string
exception Not_an_integer
exception Not_a_b58

(* TODO: this is clearly not in the right file *)
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

module With_b58_and_yojson (P : sig
  type t

  val prefix : string
  val to_raw : t -> string
  val of_raw : string -> t option
end) =
struct
  include With_b58 (P)

  include With_yojson_of_b58 (struct
    type t = P.t

    let of_b58 = of_b58
    let to_b58 = to_b58
  end)
end

let z_of_yojson json =
  match json with
  | `String string -> (
      try Z.of_string string with Invalid_argument _ -> raise Not_an_integer)
  | _ -> raise Not_a_string

let yojson_of_z z = `String (Z.to_string z)

(* TODO: this is dumb *)
let rec decode_variant l string =
  match l with
  | of_b58 :: l -> (
      match of_b58 string with
      | Some v -> Some v
      | None -> decode_variant l string)
  | [] -> None
