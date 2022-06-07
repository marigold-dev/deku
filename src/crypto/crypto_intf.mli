module type S = sig
  module Secret : sig
    type t

    val encoding : t Data_encoding.t

    val equal : t -> t -> bool

    val compare : t -> t -> int

    val to_string : t -> string

    val of_string : string -> t option
  end

  module Key : sig
    type t

    val of_secret : Secret.t -> t

    val encoding : t Data_encoding.t

    val equal : t -> t -> bool

    val compare : t -> t -> int

    val to_string : t -> string

    val of_string : string -> t option
    
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    val bin_writer_t : Bin_prot.Common.buf -> pos:int -> t -> int
    
    val __bin_read_t__ : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    val bin_reader_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    val __bin_write_t__ : Bin_prot.Common.buf -> pos:int -> t -> int

    val bin_size_t : t -> int
    val bin_shape_t : Bin_shape_lib.Bin_shape.t
    val bin_t : t -> int

  end

  module Key_hash : sig
    type t

    val of_key : Key.t -> t

    val encoding : t Data_encoding.t

    val equal : t -> t -> bool

    val compare : t -> t -> int

    val to_string : t -> string

    val of_string : string -> t option

    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    val bin_writer_t : Bin_prot.Common.buf -> pos:int -> t -> int
    
    val __bin_read_t__ : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    val bin_reader_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    val __bin_write_t__ : Bin_prot.Common.buf -> pos:int -> t -> int

    val bin_size_t : t -> int
    val bin_shape_t : Bin_shape_lib.Bin_shape.t
    val bin_t : t -> int
  end

  module Signature : sig
    type t

    val size : int

    val zero : t

    val encoding : t Data_encoding.t

    val equal : t -> t -> bool

    val compare : t -> t -> int

    val to_raw : t -> string

    val to_string : t -> string

    val of_string : string -> t option
    val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    val bin_writer_t : Bin_prot.Common.buf -> pos:int -> t -> int
    
    val __bin_read_t__ : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    val bin_reader_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
    
    val bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int
    val __bin_write_t__ : Bin_prot.Common.buf -> pos:int -> t -> int

    val bin_size_t : t -> int
    val bin_shape_t : Bin_shape_lib.Bin_shape.t
    val bin_t : t -> int
  end

  val sign : Secret.t -> BLAKE2B.t -> Signature.t

  val verify : Key.t -> Signature.t -> BLAKE2B.t -> bool

  val generate : unit -> Secret.t * Key.t
end
