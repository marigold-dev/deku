#import "imported.religo" "EURO"
type storage = EURO.t

let main = ((action, store) : (unit, storage)) : (list (operation), storage) =>
 (([] : list (operation)), EURO.add(store, EURO.one))
