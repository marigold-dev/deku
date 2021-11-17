type parameter =
  Set of int | Add of int | Subtract of int | Multiply of int | Reset

let main (param, storage : parameter * int) =
  let nop : operation list = [] in
  match param with
    Set n -> nop, n
  | Add n -> nop, storage + n
  | Subtract n -> nop, storage - n
  | Multiply n -> nop, storage * n
  | Reset -> nop, 0
