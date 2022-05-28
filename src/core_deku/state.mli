open Crypto

type t [@@deriving yojson]

type receipt = Receipt_tezos_withdraw of Ledger.Withdrawal_handle.t
[@@deriving yojson]

val empty : unit -> t

(* To allow constructing and testing the core ledgers without
   running an external VM, the initialization of the VM state
   is delayed. This leaks some abstraction about a State.t, but
   it's worth it to not break our unit tests.
   TODO: figure out a better abstraction. *)
val intialize_external_vm_state :
  External_vm.External_vm_protocol.State.t -> t -> t

val ledger : t -> Ledger.t

val contract_storage : t -> Contract_storage.t

val hash : t -> BLAKE2B.t

val apply_user_operation :
  t -> BLAKE2B.t -> User_operation.t -> t * receipt option

val apply_tezos_operation : t -> Tezos_operation.t -> t
