

open Helpers
open Tezos
type unparsed = string * string option [@@deriving yojson] (* the same as Raw.Contract.t*)

(** Address.t * entrypoint, @TODO: decide on entrypoint *)
type t = Contract_hash.t * string option [@@deriving yojson,eq, ord]
let make c = c
let of_yojson json = 
  let open Result.Syntax in
  let* (contract_hash, entrypoint) = unparsed_of_yojson json  in
  let* contract_address = Option.to_result ~none:"failed to parse contract_hash" (Contract_hash.of_string contract_hash) in
  Ok (contract_address, entrypoint)

let to_yojson (contract_hash, entrypoint) = 
  (Contract_hash.to_string contract_hash, entrypoint) 
  |> unparsed_to_yojson 
  

let to_string t = Yojson.Safe.to_string @@ to_yojson t
let of_string str = Result.to_option @@ of_yojson @@ Yojson.Safe.from_string str


(* @TODO: fetch contract from storage *)
let get_contract_opt _ = failwith "todo"


