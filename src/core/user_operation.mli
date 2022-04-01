open Crypto
type initial_operation =
  | Transaction    of {
      destination : Key_hash.t;
      amount : Amount.t;
      ticket : Ticket_id.t;
    }
  | Tezos_withdraw of {
      owner : Tezos.Address.t;
      amount : Amount.t;
      ticket : Ticket_id.t;
    }
  | Vm_transaction of { payload : Yojson.Safe.t }
type t = private {
  hash : BLAKE2B.t;
  sender : Address.t;
  initial_operation : initial_operation;
}
[@@deriving eq, ord, yojson]
val make : sender:Address.t -> initial_operation -> t
