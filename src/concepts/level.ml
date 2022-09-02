open Deku_stdlib
open N

(* TODO: should we prefix level in b58? *)
type level = N.t
and t = level [@@deriving eq, ord, yojson]

let zero = zero
let next x = x + one
let of_n n = n
let to_n n = n
let ( < ) = ( < )
let to_b58 level = level |> to_n |> N.show
let of_b58 string = string |> Z.of_string |> N.of_z |> of_n