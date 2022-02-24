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
  end
  module Key_hash : sig
    type t
    val of_key : Key.t -> t
    val encoding : t Data_encoding.t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
    val of_string : string -> t option
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
  end
  val sign : Secret.t -> BLAKE2B.t -> Signature.t
  val verify : Key.t -> Signature.t -> BLAKE2B.t -> bool
  val generate : unit -> Secret.t * Key.t
end
