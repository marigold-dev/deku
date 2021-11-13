type 'a return = operation list * 'a

let sto_plus_n (n,s: int * int) : int = s + n + 1

let main_calling ((addr,s): address * int) : int return =
  let x : int option = Tezos.call_view "sto_plus_n" 1 addr in
  let ret =
    match x with
    | None -> (failwith "view call failed" : int)
    | Some i -> i
  in
  ([]:operation list) , ret

let main_with_view (((),s): unit * int) : int return = ([]:operation list) , s