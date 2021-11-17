type parameter is
  Increment of int
| Extend of never

type storage is int

function main(const action : parameter; const store : storage) : list (operation) * storage is
  ((nil : list (operation)),
   case action of
     Increment (n) -> store + n
   | Extend (k) -> block {
       const f : (never -> int) =
        [%Michelson ({| { NEVER } |} : never -> int)];
     } with f (k)
   end)
