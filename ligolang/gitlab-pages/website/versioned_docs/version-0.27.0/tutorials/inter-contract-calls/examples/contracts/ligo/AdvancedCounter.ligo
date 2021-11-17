type parameter is
    Set of int | Add of int | Subtract of int | Multiply of int | Reset

function main (const param : parameter; const storage : int) is
block {
  const nop : list (operation) = list []
} with
    case param of [
      Set (n) -> (nop, n)
    | Add (n) -> (nop, storage + n)
    | Subtract (n) -> (nop, storage - n)
    | Multiply (n) -> (nop, storage * n)
    | Reset -> (nop, 0)
    ]
