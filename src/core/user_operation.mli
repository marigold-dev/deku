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
type t = private {
  hash : BLAKE2B.t;
  sender : Address.t;
  initial_operation : initial_operation;
}
[@@deriving eq, ord, yojson]
val make : sender:Address.t -> initial_operation -> t
