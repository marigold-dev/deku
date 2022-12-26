open Deku_stdlib
open N

(* TODO: should we prefix level in b58? *)
type level = N.t
and t = level [@@deriving show, eq, ord]

let zero = zero
let next x = x + one
let of_n n = n
let to_n n = n

let encoding =
  (* We can almost surely fit any level into 2^31
     so we convert it to number in the JSON encoding. *)
  let open Data_encoding in
  let json =
    conv
      (fun n -> N.to_z n |> Z.to_int32)
      (fun int -> Z.of_int32 int |> N.of_z |> Option.get)
      int32
  in
  splitted ~json ~binary:encoding

let ( > ) = ( > )

module Map = Map
