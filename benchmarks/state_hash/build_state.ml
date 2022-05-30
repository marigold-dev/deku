(*
  A Deku state is of:
  type t =
  {
    ledger : Ledger.t;
    contract_storage: Contract_storage.t
  }
*)

(* TODO : not complete yet *)
let build_state () =
  let ledger = Ledger.empty in
  let contract_storage = Build_contract_storage.build_contract () in
  { ledger; contract_storage }
