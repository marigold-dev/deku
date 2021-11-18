(* The function defined in "EffectfulBindingV2" is quite verbose.
   Moreover, the function *code* does not get executed when we
   define a function, so the expensive `get_contract_opt` instruction
   gets called only when the function `target_exists` is called.

   Thus, intuitively, it may be possible that we may reduce both
   the contract size and gas consumption by removing the [@inline]
   attribute. Let's check our assumption.
*)

const some_contract = ("KT1WhG8rMaC1azBJApBHW2JJdhWuhvemw4Zf" : address)

(* Calls to the function are not inlined anymore *)
function target_exists (const u : unit) is
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
