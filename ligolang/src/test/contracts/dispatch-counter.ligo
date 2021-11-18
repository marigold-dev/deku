type parameter is
  Increment of int
| Decrement of int

type storage is int

type return is list (operation) * storage

function increment (const i : int; const n : int) : int is i+n
function decrement (const i : int; const n : int) : int is i-n

const nop : list (operation) = nil

function main (const action : parameter; const store : storage) : return is
  case action of
    Increment (n) -> (nop, increment (store, n))
  | Decrement (n) -> (nop, decrement (store, n))
  end
