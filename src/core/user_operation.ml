open Helpers
open Crypto
type initial_operation =
  | Transaction          of {
      destination : Address.t;
      amount : Amount.t;
      ticket : Ticket_id.t;
    }
  | Contract_origination of {
      to_originate : Smart_contracts.Origination_payload.t;
      ticket : Ticket_id.t;
      amount : Amount.t;
    }
  | Tezos_withdraw       of {
      owner : Tezos.Address.t;
      amount : Amount.t;
      ticket : Ticket_id.t;
    }
[@@deriving yojson]
type t = {
  hash : BLAKE2B.t;
  sender : Address.t;
  initial_operation : initial_operation;
}
[@@deriving yojson]
let equal a b = BLAKE2B.equal a.hash b.hash
let compare a b = BLAKE2B.compare a.hash b.hash
let hash, verify =
  let to_yojson sender initial_operation =
    [%to_yojson: Address.t * initial_operation] (sender, initial_operation)
    |> Yojson.Safe.to_string in
  let hash sender initial_operation =
    to_yojson sender initial_operation |> BLAKE2B.hash in
  let verify hash sender initial_operation =
    to_yojson sender initial_operation |> BLAKE2B.verify ~hash in
  (hash, verify)
let make ~sender initial_operation =
  let hash = hash sender initial_operation in
  { hash; sender; initial_operation }
let verify hash sender initial_operation =
  let%assert () =
    ("Invalid_user_operation_hash", verify hash sender initial_operation) in
  Ok { hash; sender; initial_operation }
let of_yojson json =
  let%ok t = of_yojson json in
  let%ok t = verify t.hash t.sender t.initial_operation in
  Ok t
