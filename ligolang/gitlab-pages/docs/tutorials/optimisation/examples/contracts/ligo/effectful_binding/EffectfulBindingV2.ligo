(* A fixed version of "EffectfulBindingV1" – here we replace
   an effectful binding with a function. Since the function
   definition itself does not require accessing the context,
   we can inline the calls to this function.
*)

const some_contract = ("KT1WhG8rMaC1azBJApBHW2JJdhWuhvemw4Zf" : address)

(* Calls to a function can be inlined *)
[@inline] function target_exists (const u : unit) is
block {
  const c : option (contract (int)) = Tezos.get_contract_opt (some_contract)
} with
    case c of [
      Some (contract) -> True
    | None -> False
    ]

type parameter is Increment | IncrementIfEmpty | IncrementIfExists

function main (const p : parameter; const s : int) is
block {
  const nop = (list [] : list (operation))
} with
    case p of [
      Increment -> (nop, s + 1)
    | IncrementIfEmpty ->
        if target_exists (Unit) then (nop, s) else (nop, s + 1)
    | IncrementIfExists ->
        if target_exists (Unit) then (nop, s + 1) else (nop, s)
    ]
