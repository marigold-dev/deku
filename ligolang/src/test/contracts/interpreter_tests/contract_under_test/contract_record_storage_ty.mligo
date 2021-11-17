type storage = { foo : int ; bar : string}
type return = operation list * storage
type parameter = One | Two

let main (action, store : parameter * storage) : return =
   ([] : operation list), {store with foo = store.foo + 1}