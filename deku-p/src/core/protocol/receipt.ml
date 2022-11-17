open Deku_ledger

type receipt =
  | Ticket_transfer_receipt of { operation : Operation_hash.t }
  | Withdraw_receipt of {
      operation : Operation_hash.t;
      handle : Ledger.Withdrawal_handle.t;
    }
  | Vm_transaction_receipt of { operation : Operation_hash.t }

and t = receipt [@@deriving eq, yojson]

let encoding =
  let open Data_encoding in
  union ~tag_size:`Uint8
    [
      case ~title:"ticket_transfer" (Tag 0)
        (Data_encoding.dynamic_size (tup1 Operation_hash.encoding))
        (fun receipt ->
          match receipt with
          | Ticket_transfer_receipt { operation } -> Some operation
          | _ -> None)
        (fun operation -> Ticket_transfer_receipt { operation });
      case ~title:"withdraw" (Tag 1)
        (Data_encoding.dynamic_size
           (tup2 Operation_hash.encoding Ledger.Withdrawal_handle.encoding))
        (fun receipt ->
          match receipt with
          | Withdraw_receipt { operation; handle } -> Some (operation, handle)
          | _ -> None)
        (fun (operation, handle) -> Withdraw_receipt { operation; handle });
      case ~title:"vm_transaction" (Tag 2)
        (Data_encoding.dynamic_size (tup1 Operation_hash.encoding))
        (fun receipt ->
          match receipt with
          | Vm_transaction_receipt { operation } -> Some operation
          | _ -> None)
        (fun operation -> Vm_transaction_receipt { operation });
    ]
