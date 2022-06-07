type t =
  | Ed25519   of Ed25519.Signature.t
  | Secp256k1 of Secp256k1.Signature.t
  | P256      of P256.Signature.t
[@@deriving ord, eq, yojson, bin_io]

val size : int

val zero : t

val sign : Secret.t -> BLAKE2B.t -> t

val verify : Key.t -> t -> BLAKE2B.t -> bool

val to_raw : t -> string

val to_string : t -> string

val of_string : string -> t option

val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
val bin_writer_t : Bin_prot.Common.buf -> pos:int -> t -> int

val bin_reader_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
val __bin_read_t__ : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t

val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int

val __bin_write_t__ : Bin_prot.Common.buf -> pos:int -> t -> int

val bin_size_t : t -> int
val bin_shape_t : Bin_shape_lib.Bin_shape.t
val bin_t : t -> int