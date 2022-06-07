type t =
  | Ed25519   of Ed25519.Key.t
  | Secp256k1 of Secp256k1.Key.t
  | P256      of P256.Key.t
[@@deriving ord, eq, bin_io]

val of_secret : Secret.t -> t

val encoding : t Data_encoding.t

val to_string : t -> string

val of_string : string -> t option

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, string) result

val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
val bin_writer_t : Bin_prot.Common.buf -> pos:int -> t -> int

val bin_reader_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> int -> t
val __bin_read_t__ : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> int -> t

val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int

val __bin_write_t__ : Bin_prot.Common.buf -> pos:int -> t -> int

val bin_size_t : t -> int
val bin_shape_t : Bin_shape_lib.Bin_shape.t
val bin_t : t -> int