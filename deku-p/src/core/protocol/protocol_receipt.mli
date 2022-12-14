open Deku_concepts

(* TODO: make receipt generation conditional / parallel
    would allow us to have way more details in the receipt itself *)
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
  (* TODO: balance_{source,destination}_{before,after} on transfer *)
  | Internal_receipt_transfer
  | Internal_receipt_noop

type signed_receipt =
  | Signed_receipt_applied of {
      signed_operation : Protocol_operation.Signed.t;
      internal_receipt : internal_receipt;
    }
  | Signed_receipt_duplicated of {
      signed_operation : Protocol_operation.Signed.t;
    }

type block_receipt = Block_receipt of { receipts : signed_receipt list }
and t = block_receipt
(* TODO: block receipt *)
