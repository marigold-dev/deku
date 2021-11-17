open Base
include Option

(* Syntax *)
let (let*) x f = bind ~f x

(* Combinators *)
let bind_eager_or = fun a b -> match (a , b) with
  | Some a , _ -> Some a
  | _ , Some b -> Some b
  | _ -> None

let map_pair_or = fun (fa, fb) p ->
  bind_eager_or (fa p) (fb p)

let bind_union (a , b) = match (a , b) with
  | Some x , _ -> Some (`Left x)
  | None , Some x -> Some (`Right x)
  | _ -> None
let bind_pair = fun (a , b) ->
  let* a' = a in 
  let* b' = b in
  Some (a' , b')


let bind_map_pair = fun f (a , b) -> bind_pair (f a , f b)