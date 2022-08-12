open Deku_crypto
open Deku_concepts
open Deku_tezos

type internal_operation =
  | Deposit of {
      destination : Key_hash.t;
      amount : Amount.t;
      ticket : Tezos_ticket_id.t;
    }
[@@deriving yojson]

type tezos_operation = {
  hash : Tezos_operation_hash.t;
  operations : internal_operation list;
}
[@@deriving yojson]

type t = tezos_operation [@@deriving yojson]

let make hash operations =
  (*TODO: do we need to verify the hash ? here ? or somewhere else ? *)
  { hash; operations }
