let used = 1tez
let unused = 2tez

module Tezo = struct
  let used = used
  let unused = unused
  module X = struct
    let used = used
    let unused = unused
    module Y = struct
      let used = used
      let unused = unused
    end
  end
end

let used = Tezo.X.Y.used
let unused = Tezo.X.Y.unused

type parameter =
  Increment
| Decrement

type storage = tez

type return = (operation) list * storage

let main (action, _ : parameter * storage) : operation list * storage =
  (([]: operation list),
   (match action with
      Increment -> used
    | Decrement -> 1tez))                                 