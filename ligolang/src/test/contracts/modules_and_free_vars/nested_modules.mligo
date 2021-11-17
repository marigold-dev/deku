module Tezo = struct
  module X = struct
    module Y = struct
      let amoun = 1tez
    end
  end
end

let balanc = 2tez
let size = 10
let bal = balanc + 1tez
let amt = Tezo.X.Y.amoun + 1tez


type parameter =
  Increment
| Decrement

type storage = tez

type return = (operation) list * storage

let main (action, _ : parameter * storage) : operation list * storage =
  (([]: operation list),
   (match action with
      Increment -> bal
    | Decrement -> amt))                                 