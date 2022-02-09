open Crypto
type t[@@deriving yojson]
type receipt =
  | Receipt_tezos_withdraw of Ledger.Handle.t [@@deriving yojson]
val empty : t
val ledger : t -> Ledger.t
val hash : t -> BLAKE2B.t
val apply_user_operation : t -> User_operation.t -> (t * receipt option)
val apply_tezos_operation : t -> Tezos_operation.t -> t