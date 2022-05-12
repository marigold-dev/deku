open Crypto
open Helpers

type t =
  | Implicit   of Key_hash.t
  | Originated of Contract_address.t
[@@deriving eq, ord, yojson]

let is_implicit = function
  | Implicit _ -> true
  | Originated _ -> false

let is_originated t = not (is_implicit t)

let of_key_hash implicit = Implicit implicit

let to_key_hash t =
  match t with
  | Implicit implicit -> Some implicit
  | _ -> None

let to_contract_hash t =
  match t with
  | Implicit _key_hash -> None
  | Originated contract_hash -> Some contract_hash

let of_contract_hash contract_hash = Originated contract_hash

let to_string = function
  | Implicit implicit -> Key_hash.to_string implicit
  | Originated contract_hash -> Contract_address.to_string contract_hash

let of_string =
  let implicit string =
    let%some key_hash = Key_hash.of_string string in
    Some (Implicit key_hash) in
  let contract string =
    let%some contract_hash = Contract_address.of_string string in
    Some (Originated contract_hash) in
  Encoding_helpers.parse_string_variant [implicit; contract]
