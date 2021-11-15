type tr = { myt : int ticket ; mynat : nat }
type parameter = ( tr * nat option )
type storage = nat

let main = fun (p,s: parameter * storage) ->
  match p with
    | { myt = myt ; mynat = mynat } , None -> (([]: operation list), mynat)
    | { myt = myt ; mynat = mynat } , Some x -> (([]: operation list), x)