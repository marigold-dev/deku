open Datatypes

module Pos =
 struct
  (** val succ : Zarith.t -> Zarith.t **)

  let rec succ x =
    (fun b1 b2 b3 x -> Zarith.(if x = one then b3 () else let (q, r) = ediv_rem x (of_int 2) in if r = zero then b2 q else b1 q))
      (fun p -> (fun x -> Zarith.mul (Zarith.add Zarith.one Zarith.one) x)
      (succ p))
      (fun p ->
      (fun x -> Zarith.add Zarith.one (Zarith.mul (Zarith.add Zarith.one Zarith.one) x))
      p)
      (fun _ -> (fun x -> Zarith.mul (Zarith.add Zarith.one Zarith.one) x) Zarith.one)
      x

  (** val of_succ_nat : nat -> Zarith.t **)

  let rec of_succ_nat = function
  | O -> Zarith.one
  | S x -> succ (of_succ_nat x)
 end
