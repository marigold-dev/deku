open Helpers
open Crypto
type validator = { address : Key_hash.t } [@@deriving eq, ord, yojson]
type t = {
  current : validator option;
  validators : validator list;
  length : int;
  hash : BLAKE2B.t;
}
[@@deriving yojson]
let current t = t.current
let to_list t = t.validators
let length t = t.length
let after_current n t =
  let%some current_producer = t.current in
  let%some current_index =
    List.find_index (( = ) current_producer) t.validators in
  let relative_index = (current_index + n) mod t.length in
  let index =
    match relative_index < 0 with
    | true -> t.length + relative_index
    | false -> relative_index in
  List.nth_opt t.validators index
let update_current address t =
  let validator =
    t.validators |> List.find_opt (fun validator -> validator.address = address)
  in
  { t with current = validator }
let hash_validators validators =
  validators
  |> List.map (fun validator -> validator.address)
  |> Tezos.Deku.Consensus.hash_validators
let empty =
  { current = None; validators = []; length = 0; hash = hash_validators [] }
let add validator t =
  let validators =
    t.validators @ [validator] |> List.in_order_uniq compare_validator in
  let new_proposer =
    match t.current = None with
    | true -> Some validator
    | false -> t.current in
  let hash = hash_validators validators in
  { current = new_proposer; validators; length = List.length validators; hash }
let remove validator t =
  let validators = t.validators |> List.filter (( <> ) validator) in
  let length = List.length validators in
  let current =
    match (validators, t.current, validator) with
    | [], _, _ -> None
    | _, Some current, removed when current = removed -> after_current 1 t
    | _ -> t.current in
  let hash = hash_validators validators in
  { current; validators; length; hash }
let hash t = t.hash
