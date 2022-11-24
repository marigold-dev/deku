open Deku_stdlib
open Deku_crypto

type operation_hash
type t = operation_hash [@@deriving eq, ord, show]

(* repr *)
val to_blake2b : operation_hash -> BLAKE2b.t
val of_b58 : string -> operation_hash option
val to_b58 : operation_hash -> string
val encoding : operation_hash Data_encoding.t

(* operations *)
val hash : string -> operation_hash

module Map : Map.S with type key = operation_hash
