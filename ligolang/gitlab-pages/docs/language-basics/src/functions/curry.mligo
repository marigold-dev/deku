let add (a, b : int * int) : int = a + b            // Uncurried
let add_curry (a : int) (b : int) : int = add (a,b) // Curried
let increment (b : int) : int = add_curry 1         // Partial application
