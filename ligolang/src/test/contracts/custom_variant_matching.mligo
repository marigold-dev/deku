type my_option = B of unit | A | C of string

let a = C "test"

let b = A

let c () =
  match b with
    C _ -> 0
  | A -> 1
  | _ -> 2
