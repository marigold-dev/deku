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
type t = private {
  hash : BLAKE2B.t;
  source : Key_hash.t;
  initial_operation : initial_operation;
}
[@@deriving eq, ord, yojson]
val make : source:Key_hash.t -> initial_operation -> t
