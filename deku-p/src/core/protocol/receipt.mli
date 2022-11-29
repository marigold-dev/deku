open Deku_ledger

type receipt =
  | Ticket_transfer_receipt of { operation : Operation_hash.t }
  | Withdraw_receipt of {
      operation : Operation_hash.t;
      handle : Ledger.Withdrawal_handle.t;
    }
  | Vm_origination_receipt of {
      operation : Operation_hash.t;
      contract_address : Deku_ledger.Contract_address.t;
    }
  | Vm_transaction_receipt of {
      operation : Operation_hash.t;
      contract_address : Deku_ledger.Contract_address.t;
    }
  | Vm_transaction_error of { operation : Operation_hash.t; message : string }

type t = receipt [@@deriving eq, yojson]

val encoding : receipt Data_encoding.t
