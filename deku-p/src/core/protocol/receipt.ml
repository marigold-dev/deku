open Deku_ledger

type receipt =
  | Ticket_transfer_receipt of { operation : Operation_hash.t }
  | Withdraw_receipt of {
      operation : Operation_hash.t;
      handle : Ledger.Withdrawal_handle.t; [@opaque]
    }
  | Gameboy_receipt of { data : Deku_gameboy.frame_buffer }
  | Vm_transaction_receipt of { operation : Operation_hash.t }

and t = receipt [@@deriving eq, yojson, show]
