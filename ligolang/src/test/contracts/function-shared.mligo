(* Test use of multiple subroutines in a CameLIGO function *)

let foo (i : int) : int = i + 20
let bar (i : int) : int = i + 50
let foobar (i : int) : int = foo i + bar i
