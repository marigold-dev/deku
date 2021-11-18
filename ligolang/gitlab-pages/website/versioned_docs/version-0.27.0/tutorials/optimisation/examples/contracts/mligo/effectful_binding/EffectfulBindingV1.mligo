(* Quite an artificial example that shows how you might want to
   inline a declaration to avoid calling an expensive `get_contract_opt`
   instruction upon every contract invocation. Unfortunately, this
   will not work: the declaration has a side-effect, so it must be
   executed before running the main contract code
*)

let some_contract = ("KT1WhG8rMaC1azBJApBHW2JJdhWuhvemw4Zf" : address)

(* The inline attribute will be ignored! *)
[@inline]
let target_exists =
  let c : int contract option = Tezos.get_contract_opt some_contract in
  match c with
    Some contract -> true
  | None -> false

type parameter = Increment | IncrementIfEmpty | IncrementIfExists

let main (p, s : parameter * int) =
  let nop = ([] : operation list) in
  match p with
    Increment -> nop, s + 1
  | IncrementIfEmpty -> if target_exists then nop, s else nop, s + 1
  | IncrementIfExists -> if target_exists then nop, s + 1 else nop, s
