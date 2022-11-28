open Deku_stdlib
open Deku_crypto

type tezos_contract =
  | Tezos_contract_implicit of Key_hash.t
  | Tezos_contract_originated of Tezos_contract_hash.t

and t = tezos_contract [@@deriving show, eq, ord]

let of_key_hash key_hash = Tezos_contract_implicit key_hash

let of_tezos_contract_hash tezos_contract_hash =
  Tezos_contract_originated tezos_contract_hash

let encoding =
  let open Data_encoding in
  def "contract_id" ~title:"A contract handle"
    ~description:
      "A contract notation as given to an RPC or inside scripts. Can be a \
       base58 implicit contract hash or a base58 originated contract hash."
  @@ union ~tag_size:`Uint8
       [
         case (Tag 0) ~title:"Implicit" Key_hash.encoding
           (function Tezos_contract_implicit k -> Some k | _ -> None)
           (fun k -> Tezos_contract_implicit k);
         case (Tag 1)
           (Fixed.add_padding Tezos_contract_hash.encoding 1)
           ~title:"Originated"
           (function
             | Tezos_contract_originated contract -> Some contract | _ -> None)
           (fun contract -> Tezos_contract_originated contract);
       ]

let to_b58 = function
  | Tezos_contract_implicit key_hash -> Key_hash.to_b58 key_hash
  | Tezos_contract_originated contract -> Tezos_contract_hash.to_b58 contract

let of_b58 =
  let implicit string =
    let%some implicit = Key_hash.of_b58 string in
    Some (Tezos_contract_implicit implicit)
  in
  let originated string =
    let%some contract = Tezos_contract_hash.of_b58 string in
    Some (Tezos_contract_originated contract)
  in
  Deku_repr.decode_variant [ implicit; originated ]
