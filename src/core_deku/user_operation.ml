open Helpers
open Crypto

type initial_operation =
  | Transaction          of {
      destination : Key_hash.t;
      amount : Amount.t;
      ticket : Ticket_id.t;
    }
  | Contract_invocation  of {
      to_invoke : Contract_address.t;
      argument : Contract_vm.Invocation_payload.t;
      tickets : (Ticket_id.t * Amount.t) list;
    }
  | Contract_origination of {
      payload : Contract_vm.Origination_payload.t;
      tickets : (Ticket_id.t * Amount.t) list;
    }
  | Tezos_withdraw       of {
      owner : Tezos.Address.t;
      amount : Amount.t;
      ticket : Ticket_id.t;
    }
[@@deriving yojson]
type t = {
  hash : BLAKE2B.t;
  source : Key_hash.t;
  initial_operation : initial_operation;
}
[@@deriving yojson]
let equal a b = BLAKE2B.equal a.hash b.hash
let compare a b = BLAKE2B.compare a.hash b.hash
let hash, verify =
  let to_yojson sender initial_operation =
    [%to_yojson: Key_hash.t * initial_operation] (sender, initial_operation)
    |> Yojson.Safe.to_string in
  let hash sender initial_operation =
    to_yojson sender initial_operation |> BLAKE2B.hash in
  let verify hash sender initial_operation =
    to_yojson sender initial_operation |> BLAKE2B.verify ~hash in
  (hash, verify)

let make ~source initial_operation =
  let hash = hash source initial_operation in
  { hash; source; initial_operation }
let verify hash source initial_operation =
  let%assert () =
    ("Invalid_user_operation_hash", verify hash source initial_operation) in
  Ok { hash; source; initial_operation }
let of_yojson json =
  let%ok t = of_yojson json in
  let%ok t = verify t.hash t.source t.initial_operation in
  Ok t
