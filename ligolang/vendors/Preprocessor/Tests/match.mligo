type mytype = Foo of int | Bar of string

let a = 1

let b =
  match (let c = a in Foo c) with
  | Foo x -> x + a
  | Bar y -> 1 + a

let c =
  match ([ 1 ; 2 ;3 ]) with
  | [] ->
    let c = 2 in
    a
  | hd::tl -> 2

let d =
  match (let d = 1 in Some (a+d)) with
  | Some (s) -> s + a
  | None -> a