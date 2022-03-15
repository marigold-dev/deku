(* TODO: satured arithmetic *)
type t = Burn of int ref | Measure of int ref

let constant_burn = 100
let log2_burn ~cardinality =
  let log2_cardinality =
    let open Float in
    to_int (ceil (log2 (of_int cardinality))) in
  (* additional burn to prevent free operations when cardinality = 0 *)
  constant_burn + (constant_burn * log2_cardinality)

let make ~initial_gas = Burn (ref initial_gas)

let measure () = Measure (ref 0)

let is_empty t =
  match t with
  | Burn t -> !t <= 0
  | Measure _ -> false

let burn t amount =
  match t with
  | Burn t -> t := !t - amount
  | Measure t -> t := !t + amount

let current t =
  match t with
  | Measure t
  | Burn t -> !t

let burn_constant t = burn t constant_burn
let burn_log2 t ~cardinality = burn t (log2_burn ~cardinality)
