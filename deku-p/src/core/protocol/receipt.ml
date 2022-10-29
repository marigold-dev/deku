open Deku_ledger
open Deku_concepts

type receipt =
  | Ticket_transfer_receipt of {
      operation : Operation_hash.t;
      sender : Address.t;
      receiver : Address.t;
      ticket_id : Ticket_id.t;
      amount : Amount.t;
    }
  | Withdraw_receipt of {
      operation : Operation_hash.t;
      handle : Ledger.Withdrawal_handle.t;
      account : Address.t;
      ticket_id : Ticket_id.t;
      amount : Amount.t;
    }
  | Vm_transaction_receipt of { operation : Operation_hash.t }

and t = receipt [@@deriving eq, yojson]
