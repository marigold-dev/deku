type foo =
  Bar of int
| Baz

let main (f : foo) : int =
  match f with
    Bar i -> i
  | Baz -> -1
