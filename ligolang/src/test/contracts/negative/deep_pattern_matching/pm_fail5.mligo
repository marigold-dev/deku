type foo = None_fake | Some_fake of int

let t = fun (x: int option) ->
  match x with
  | Some_fake x -> x
  | None_fake -> 1