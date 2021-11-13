type 'a return = operation list * 'a

[@view] let v1 (n,s: int * int) : int = s + n + 1
let v2 (_,s: int * int) : int = s + 2
let bad_view (_,_: int * nat ) : nat = 1n

let main (((),s): unit * int) : int return = ([]:operation list) , s