open Z_ext

type nat = Z_ext.t
and t = nat [@@deriving eq, ord, yojson]

let show n = Format.asprintf "%a" pp_print n
let pp = pp_print
let check n = match n < zero with true -> None | false -> Some n

exception Not_a_natural

let t_of_yojson json =
  match check (t_of_yojson json) with
  | Some n -> n
  | None -> raise Not_a_natural

let zero = zero
let one = one
let of_z x = check x
let to_z x = x
let encoding = Data_encoding.(dynamic_size n)
let ( + ) a b = a + b
let ( - ) a b = check (a - b)
let ( > ) a b = Z.gt a b

module Map = Map_ext.Make (struct
  type t = nat [@@deriving ord, yojson]

  let encoding = encoding
end)
