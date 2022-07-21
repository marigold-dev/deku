open Crypto

type t [@@deriving yojson]

type contract_invocation_changes

and contract_origination_changes

and receipt =
  | Receipt_tezos_withdraw       of Ledger.Withdrawal_handle.t
  | Receipt_contract_origination of {
      sender : Address.t;
      outcome : [`Success of contract_origination_changes | `Failure];
    }
  | Receipt_contract_invocation  of {
      sender : Address.t;
      outcome : [`Success of contract_invocation_changes | `Failure];
    }
[@@deriving yojson]

val empty : t

val ledger : t -> Ledger.t

val contract_storage : t -> Contract_storage.t

val hash : t -> BLAKE2B.t

val apply_user_operation :
  t -> BLAKE2B.t -> User_operation.t -> t * receipt option

val apply_tezos_operation : t -> Tezos_operation.t -> t
