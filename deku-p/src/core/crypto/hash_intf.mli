open Deku_stdlib
open Deku_repr

module type Alg = sig
  type secret
  type key
  type signature

  val sign : secret -> string -> signature
  val verify : key -> signature -> string -> bool
end

module type S = sig
  type hash
  type t = hash [@@deriving show, eq, ord]

  val hash : string -> hash

  (* repr *)
  val of_hex : string -> hash option
  val to_hex : hash -> string
  val of_raw : string -> hash option
  val to_raw : hash -> string

  (* utils *)
  val both : hash -> hash -> hash
  val all : hash list -> hash
  val digest_size : int

  (* TODO: better name for this*)
  module With_alg (Alg : Alg) : sig
    open Alg

    val sign : secret -> hash -> signature
    val verify : key -> signature -> hash -> bool
  end

  module With_b58_and_encoding_and_yojson (_ : sig
    val name : string
    val prefix : Prefix.t
  end) : sig
    val of_b58 : string -> hash option
    val to_b58 : hash -> string
    val encoding : hash Data_encoding.t
    val t_of_yojson : [> `String of string ] -> hash
    val yojson_of_t : hash -> [> `String of string ]

    module Set : Set.S with type elt = hash
    module Map : Map.S with type key = hash
  end
end
