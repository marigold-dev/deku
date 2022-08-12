open Deku_crypto

type address = Implicit of Key_hash.t | Originated of Contract_address.t
and t = address [@@deriving eq, ord, yojson]

let is_implicit address =
  match address with Implicit _ -> true | Originated _ -> false

let is_originated address =
  match address with Implicit _ -> false | Originated _ -> true

let of_key_hash key_hash = Implicit key_hash
let of_contract_hash contract_hash = Originated contract_hash

let to_key_hash address =
  match address with Implicit address -> Some address | Originated _ -> None

let to_contract_hash address =
  match address with
  | Implicit _ -> None
  | Originated contract_hash -> Some contract_hash

let of_b58 string =
  let implicit string =
    string |> Key_hash.of_b58 |> Option.map (fun key_hash -> Implicit key_hash)
  in
  let originated string =
    string |> Contract_address.of_b58
    |> Option.map (fun contract_hash -> Originated contract_hash)
  in
  [ implicit; originated ] |> List.find_map (fun f -> f string)

let to_b58 address =
  match address with
  | Implicit key_hash -> Key_hash.to_b58 key_hash
  | Originated contract_hash -> Contract_address.to_b58 contract_hash

module Map = Map.Make (struct
  type t = address

  let compare = compare
end)
