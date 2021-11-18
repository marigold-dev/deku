type parameter is Add of int | Subtract of int

type storage is int

function main (const p : parameter; const s : storage) is
block {
  const nop : list (operation) = list []
} with
    case p of [
      Add (n) -> (nop, s + n)
    | Subtract (n) -> (nop, s - n)
    ]
