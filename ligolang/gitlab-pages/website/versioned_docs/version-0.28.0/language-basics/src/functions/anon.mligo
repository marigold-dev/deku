let increment (b : int) : int = (fun (a : int) -> a + 1) b
let a : int = increment 1 // a = 2
