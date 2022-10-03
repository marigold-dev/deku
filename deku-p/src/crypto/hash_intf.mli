open Deku_stdlib

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
  val digest_size : int

  (* TODO: better name for this*)
  module With_alg (Alg : Alg) : sig
    open Alg

    val sign : secret -> hash -> signature
    val verify : key -> signature -> hash -> bool
  end

  module With_b58 (_ : sig
    val prefix : string
  end) : sig
    val of_b58 : string -> hash option
    val to_b58 : hash -> string
  end

  module Set : Set.S with type elt = hash
  module Map : Map.S with type key = hash
end
