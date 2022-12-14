open Deku_stdlib
open N

(* TODO: should we prefix level in b58? *)
type level = N.t
and t = level [@@deriving show, eq, ord, yojson]

let zero = zero
let next x = x + one
let of_n n = n
let to_n n = n

let encoding =
  let open Data_encoding in
  (* TODO: why did I make level fixed size? Explain here *)
  conv_with_guard
    (fun level ->
      let level = N.to_z level in
      (* TODO: implicit exception here *)
      Z.to_int64 level)
    (fun level ->
      let level = Z.of_int64 level in
      match N.of_z level with
      | Some level -> Ok level
      | None -> Error "too big of a level")
    int64

let ( > ) = ( > )

module Map = Map
