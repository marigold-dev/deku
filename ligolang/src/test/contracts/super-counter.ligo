type action is
  Increment of int
| Decrement of int

type storage is int

type return is list (operation) * storage

function main (const p : action; const s : int) : return is
  ((nil : list (operation)),
   case p of
     Increment (n) -> s + n
   | Decrement (n) -> s - n
   end)
