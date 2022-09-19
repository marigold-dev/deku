type receipt =
  | Transaction_receipt of { operation : Operation_hash.t }
  | Withdraw_receipt of Ledger.Withdrawal_handle.t

type t = receipt [@@deriving eq]
