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

and t = receipt [@@deriving eq]

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
      case ~title:"vm_origination" (Tag 2)
        (Data_encoding.dynamic_size
           (tup2 Operation_hash.encoding Deku_ledger.Contract_address.encoding))
        (fun receipt ->
          match receipt with
          | Vm_origination_receipt { operation; contract_address } ->
              Some (operation, contract_address)
          | _ -> None)
        (fun (operation, contract_address) ->
          Vm_origination_receipt { operation; contract_address });
      case ~title:"vm_transaction" (Tag 3)
        (Data_encoding.dynamic_size
           (tup2 Operation_hash.encoding Deku_ledger.Contract_address.encoding))
        (fun receipt ->
          match receipt with
          | Vm_transaction_receipt { operation; contract_address } ->
              Some (operation, contract_address)
          | _ -> None)
        (fun (operation, contract_address) ->
          Vm_transaction_receipt { operation; contract_address });
      case ~title:"vm_transaction_error" (Tag 4)
        (Data_encoding.dynamic_size (tup2 Operation_hash.encoding string))
        (fun receipt ->
          match receipt with
          | Vm_transaction_error { operation; message } ->
              Some (operation, message)
          | _ -> None)
        (fun (operation, message) ->
          Vm_transaction_error { operation; message });
    ]
