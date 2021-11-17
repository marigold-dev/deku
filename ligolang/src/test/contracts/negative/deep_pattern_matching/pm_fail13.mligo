type storage = int
type action = | Increment of nat | Decrement of nat
let main (p,s: action * storage) =
 let stor =
   match p with 
   | Increment n -> s +1
   | Decrement -> s -1
 in ([] : operation list), stor