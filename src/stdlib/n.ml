open Deku_repr
open Z

type nat = Z.t
and t = nat [@@deriving eq, ord]

let show n = Format.asprintf "%a" pp_print n
let pp = pp_print
let check n = match n < zero with true -> None | false -> Some n

exception Not_a_natural

let yojson_of_t = yojson_of_z

let t_of_yojson json =
  match check (z_of_yojson json) with
  | Some n -> n
  | None -> raise Not_a_natural

let zero = zero
let one = one
let of_z x = check x
let to_z x = x
let ( + ) a b = a + b
let ( - ) a b = check (a - b)
let ( < ) a b = a < b

module Map = Map.Make (struct
  type t = nat

  let compare = compare
end)
