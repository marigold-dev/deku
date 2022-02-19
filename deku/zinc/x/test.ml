let add a b = a + b

let dcr a b = a - b

type parameter = Increment of int | Decrement of int | Reset

let main parameter storage =
  match parameter with
  | Increment n -> add n storage
  | Decrement n -> dcr n storage
  | Reset -> 0
