exception Out_of_gas
type error = [`Out_of_gas] [@@deriving show]

(* TODO: satured arithmetic *)
type t = int ref
let constant_burn = 100
let log2_burn ~cardinality =
  let log2_cardinality =
    let open Float in
    to_int (ceil (log2 (of_int cardinality))) in
  (* additional burn to prevent free operations when cardinality = 0 *)
  constant_burn + (constant_burn * log2_cardinality)

let make ~initial_gas = ref initial_gas

let to_int t = !t

let is_empty t = !t <= 0

let burn t amount =
  let gas = !t in
  if gas >= amount then
    t := gas - amount
  else
    raise Out_of_gas

let burn_constant t = burn t constant_burn
let burn_log2 t ~cardinality = burn t (log2_burn ~cardinality)
