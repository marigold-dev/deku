open Crypto

type validator = { address : Key_hash.t } [@@deriving eq, ord, yojson, bin_io]

type t [@@deriving yojson, bin_io]

val current : t -> validator option

val to_list : t -> validator list

val length : t -> int

val after_current : int -> t -> validator option
  [@@ocaml.doc
    " [after_current(n, t)] cycle validators starting after the current one\n\
    \  best: O(1)\n\
    \  worst: O(n) "]

val update_current : Key_hash.t -> t -> t

val empty : t

val add : validator -> t -> t

val remove : validator -> t -> t

val hash : t -> BLAKE2B.t
