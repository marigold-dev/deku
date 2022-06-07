open Crypto

type t = Key.t [@@deriving yojson, ord, bin_io]

val of_key : Secret.t -> t

val genesis_key : Secret.t

val genesis_wallet : t

val to_string : t -> string

val of_string : string -> t option

val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
val bin_writer_t : Bin_prot.Common.buf -> pos:int -> t -> int

val bin_reader_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> int -> t
val __bin_read_t__ : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> int -> t

val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int

val __bin_write_t__ : Bin_prot.Common.buf -> pos:int -> t -> int

val bin_size_t : t -> int
val bin_shape_t : Bin_shape_lib.Bin_shape.t
val bin_t : t -> int
