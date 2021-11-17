(* A fixed version of "EffectfulBindingV1" – here we replace
   an effectful binding with a function. Since the function
   definition itself does not require accessing the context,
   we can inline the calls to this function.
*)

let some_contract = ("KT1WhG8rMaC1azBJApBHW2JJdhWuhvemw4Zf" : address)

(* Calls to a function can be inlined *)
[@inline]
let target_exists () =
  let c : int contract option = Tezos.get_contract_opt some_contract in
  match c with
    Some contract -> true
  | None -> false

type parameter = Increment | IncrementIfEmpty | IncrementIfExists

let main (p, s : parameter * int) =
  let nop = ([] : operation list) in
  match p with
    Increment -> nop, s + 1
  | IncrementIfEmpty -> if target_exists () then nop, s else nop, s + 1
  | IncrementIfExists -> if target_exists () then nop, s + 1 else nop, s
