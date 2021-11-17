type parameter is Compute of int -> int | Set of int

type storage is int

function main (const p : parameter; const s : storage) is
block {
  const nop : list (operation) = list []
} with
    case p of [
      Compute (func) -> (nop, func (s))
    | Set (n) -> (nop, n)
    ]
