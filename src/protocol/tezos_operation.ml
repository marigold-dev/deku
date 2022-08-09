open Deku_crypto
open Deku_concepts

type internal_operation =
  | Deposit of {
      destination : Key_hash.t;
      amount : Amount.t;
      ticket : Deku_tezos.Ticket_id.t;
    }
[@@deriving yojson]

type tezos_operation = {
  hash : Deku_tezos.Operation_hash.t;
  operations : internal_operation list;
}
[@@deriving yojson]

type t = tezos_operation [@@deriving yojson]

let make hash operations =
  (*TODO: do we need to verify the hash ? here ? or somewhere else ? *)
  { hash; operations }
