open Deku_crypto
open Deku_stdlib
open Deku_tezos

type contract =
  | Contract_implicit of Key_hash.t
  | Contract_tezos_originated of Tezos_contract_hash.t
  | Contract_deku_originated of Deku_contract_address.t

and t = contract [@@deriving show, eq, ord]

let of_key_hash key_hash = Contract_implicit key_hash

let of_tezos_contract_hash tezos_contract_hash =
  Contract_tezos_originated tezos_contract_hash

let of_deku_contract_address deku_contract_address =
  Contract_deku_originated deku_contract_address

let of_tezos_contract tezos_contract =
  match tezos_contract with
  | Tezos_contract.Tezos_contract_implicit key_hash -> of_key_hash key_hash
  | Tezos_contract.Tezos_contract_originated tezos_contract_hash ->
      of_tezos_contract_hash tezos_contract_hash

let encoding =
  let open Data_encoding in
  def "address" ~title:"A deku_address"
  @@ union ~tag_size:`Uint8
       [
         case (Tag 0) ~title:"Contract_implicit" Key_hash.encoding
           (function Contract_implicit k -> Some k | _ -> None)
           (fun k -> Contract_implicit k);
         case (Tag 1)
           (Fixed.add_padding Tezos_contract_hash.encoding 1)
           ~title:"Contract_tezos_originated"
           (function
             | Contract_tezos_originated contract -> Some contract | _ -> None)
           (fun contract -> Contract_tezos_originated contract);
         case (Tag 2)
           (Fixed.add_padding Deku_contract_address.encoding 1)
           ~title:"Contract_deku_originated"
           (function
             | Contract_deku_originated contract -> Some contract | _ -> None)
           (fun contract -> Contract_deku_originated contract);
       ]

let to_b58 = function
  | Contract_implicit key_hash -> Key_hash.to_b58 key_hash
  | Contract_tezos_originated tezos_contract_hash ->
      Tezos_contract_hash.to_b58 tezos_contract_hash
  | Contract_deku_originated deku_contract_address ->
      Deku_contract_address.to_b58 deku_contract_address

let of_b58 =
  let implicit string =
    let%some implicit = Key_hash.of_b58 string in
    Some (Contract_implicit implicit)
  in
  let tezos_originated string =
    let%some contract = Tezos_contract_hash.of_b58 string in
    Some (Contract_tezos_originated contract)
  in
  let deku_originated string =
    let%some contract = Deku_contract_address.of_b58 string in
    Some (Contract_deku_originated contract)
  in
  Deku_repr.decode_variant [ implicit; tezos_originated; deku_originated ]
