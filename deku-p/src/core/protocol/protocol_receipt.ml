open Deku_concepts

type internal_receipt =
  | Internal_receipt_ok of {
      operation : Protocol_operation.Internal.t;
      outcome : internal_receipt_desc;
    }
  | Internal_receipt_error of {
      operation : Protocol_operation.Internal.t;
      exception_ : exn;
    }

and internal_receipt_desc =
  | Internal_receipt_register of { registered_code : Ledger_code.t }
  | Internal_receipt_transfer
  | Internal_receipt_noop

and signed_receipt =
  | Signed_receipt_applied of {
      signed_operation : Protocol_operation.Signed.t;
      internal_receipt : internal_receipt;
    }
  | Signed_receipt_duplicated of {
      signed_operation : Protocol_operation.Signed.t;
    }

type block_receipt = Block_receipt of { receipts : signed_receipt list }
and t = block_receipt
