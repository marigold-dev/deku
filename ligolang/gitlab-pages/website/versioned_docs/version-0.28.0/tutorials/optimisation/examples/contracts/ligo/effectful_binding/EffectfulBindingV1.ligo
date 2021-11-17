(* Quite an artificial example that shows how you might want to
   inline a declaration to avoid calling an expensive `get_contract_opt`
   instruction upon every contract invocation. Unfortunately, this
   will not work: the declaration has a side-effect, so it must be
   executed before running the main contract code
*)

const some_contract = ("KT1WhG8rMaC1azBJApBHW2JJdhWuhvemw4Zf" : address)

(* The inline attribute will be ignored! *)
[@inline]
const target_exists
= block {
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
    | IncrementIfEmpty -> if target_exists then (nop, s) else (nop, s + 1)
    | IncrementIfExists -> if target_exists then (nop, s + 1) else (nop, s)
    ]
