open Crypto
open Helpers
type t =
  | Implicit   of Key_hash.t
  | Originated of Tezos.Contract_hash.t
[@@deriving eq, ord, yojson]
let of_key_hash implicit = Implicit implicit
let to_key_hash t =
  match t with
  | Implicit implicit -> Some implicit
  | _ -> None

let is_implicit = function
  | Implicit _ -> true
  | Originated _ -> false

let to_contract_hash t =
  match t with
  | Implicit _key_hash -> None
  | Originated contract_hash -> Some contract_hash
let of_contract_hash contract_hash = Originated contract_hash
let to_string = function
  | Implicit implicit -> Key_hash.to_string implicit
  | Originated contract_hash -> Tezos.Contract_hash.to_string contract_hash
let of_string =
  let implicit string =
    let%some key_hash = Key_hash.of_string string in
    Some (Implicit key_hash) in
  let contract string =
    let%some contract_hash = Tezos.Contract_hash.of_string string in
    Some (Originated contract_hash) in
  Encoding_helpers.parse_string_variant [implicit; contract]
