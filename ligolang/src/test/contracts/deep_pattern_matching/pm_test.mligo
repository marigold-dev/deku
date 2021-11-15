type myt = Nil | Cons of (int * int)
type myr = { a : int ; b : nat ; c : string }
type myd = One of myt | Two of myr

let t1 = fun (x: myt * myt) ->
  let fr = fun (x: myt) -> 1 in
  let fl = fun (x: myt) -> 2 in
  match x with
  | Nil , ys  -> fr ys
  | xs  , Nil -> fl xs
  | Cons (a,b) , Cons (c,d) -> a + b + c + d

// this essentially test variable substitution (see var rule)
let t2 = fun (x: myt) (y: myt) ->
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
      | _ -> a + b
    in
    a + old_b + b

let t3 = fun (x : myd) ->
  match x with
  | One (Nil) -> 1
  | One x -> (
    match x with
    | Nil -> 2
    | Cons (a,b) -> a + b
  )
  | Two { a ; b ; c  } -> a + (int b) + (int (String.length c))

(* A possible bug in the self pass (?) *)
let t2_3 = fun (x: myt) (y: myt) (x2: myd) ->
  let t2 =
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
        | Cons (a,b) -> a + b
      in
      a + old_b + b
  in
  let t3 =
    match x2 with
    | One (Nil) -> 1
    | One x -> (
      match x with
      | Nil -> 2
      | Cons (a,b) -> a + b
    )
    | Two { a = a ; b = b ; c = c } -> a + (int b) + (int (String.length c))
  in
  t2 + t3

let t4 = fun (x: myd) (y: myd) ->
  match (x , y) with
  | a , One (x) -> 1
  | One (Nil) , y -> 2
  | One (Cons(a,b)) , y -> a + b
  | Two {a=a;b=b;c=c} , Two {a=aa;b=_;c=cc} ->
    a  + (int b) + (int (String.length c)) + aa + (int (String.length cc))

let t5 = fun (x: int) ->
  match (x, ()) with
  | a , () -> a

let t6 = fun (x: int) ->
  match (x, ()) with
  | _ , _ -> 2

let t7 = fun (x: int option) ->
  match x with
  | Some x -> x
  | None -> 1

let t8 = fun (x: (int * int) option) (y: int) ->
  match x , y with
  | None , x -> x
  | Some (x,y) , _ -> x + y

let t9 = fun (x: int option) (y: int option) ->
  match x , y with
  | None , ys  -> 1
  | xs  , None -> 2
  | Some a , Some b -> a + b

type optioni = int option
type myti = Nili | Consi of optioni

let fl = fun (x:myti) -> 1
let fo = fun (x:optioni) -> 2

let t10 = fun (x: myti) (y: myti) ->
  match x,y with
  | Nili , ys  -> fl ys
  | xs  , Nili -> fl xs
  | Consi (None) , Consi (Some b) ->
    let b = 1 in b
  | Consi (a) , Consi (b) -> (fo a) + (fo b)

let t11 = fun (x: myti) (y: myti) ->
  match x,y with
  | Nili , ys  -> fl ys
  | xs  , Nili -> fl xs
  | Consi (Some a) , Consi (Some b) ->
    let a = 1 in a + b
  | Consi (a) , Consi (b) -> (
    match a with
    | None -> (fo a) + (fo b)
    | Some a -> a
  )

let t12 = fun (x : int list) ->
  match x with
  | [] -> 0
  | hd::[] -> hd
  | hd::(hd2::[]) -> hd + hd2
  | hd::(hd2::(hd3::[])) -> hd + hd2 + hd3
  | hd::tl -> -1

type recordi = { a : int list option ; b : int list }

let none_a = { a = (None:int list option) ; b = [42] }
let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }
let b_empty_a_not = { a = Some ([222]) ; b = ([]: int list) }

let t13 = fun (x:recordi) (y:recordi) ->
  match (x,y) with
  | {a=None;b=_} , { a = _ ; b = _ } -> -1
  | {a=_;b=_} , { a = Some ([]) ; b = (hd::tl) } -> hd
  | {a=_;b=_} , { a = Some (hd::tl) ; b = [] } -> hd
  | {a=Some a;b=_} , _ -> int (List.length a)