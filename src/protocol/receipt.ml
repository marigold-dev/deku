type receipt =
  | Transaction_receipt of { operation : Operation_hash.t }
  | Withdraw_receipt of Ledger.Withdrawal_handle.t

and t = receipt [@@deriving eq, yojson]
