type myt = Nil | Cons of (int * int)

let t = fun (x: myt) (y: myt) ->
  match x with
  | Nil -> (
    match y with
    | Nil -> 1
    | Cons (a,b) ->
      let a = "a" in
      (int (String.length a)) + b
  )
  | Cons (a,b) ->
    let old_b = b in
    let b =
      match y with
      | Nil ->
        let f = fun (b:int) -> b + a in
        f (b+1)
      | Cons (a,b) -> "invalid"
    in
    a + old_b + b