#import "imported.mligo" "EURO"
type storage = EURO.t

let main (action, store : unit * storage) : operation list * storage =
 (([] : operation list), EURO.add(store, EURO.one))
