module type S = sig
  module Secret : sig
    type secret
    type t = secret [@@deriving eq, ord, yojson]

    (* repr *)
    val of_b58 : string -> secret option
    val to_b58 : secret -> string

    (* utils *)
    val generate : unit -> secret

    val cmdliner_converter :
      (string -> [> `Ok of t | `Error of string ])
      * (Format.formatter -> t -> unit)
  end

  module Key : sig
    type key
    type t = key [@@deriving eq, ord, yojson]

    (* repr *)
    val of_b58 : string -> key option
    val to_b58 : key -> string
    val encoding : key Data_encoding.t

    (* operations *)
    val of_secret : Secret.t -> key

    val cmdliner_converter :
      (string -> [> `Ok of t | `Error of string ])
      * (Format.formatter -> t -> unit)
  end

  module Key_hash : sig
    type key_hash
    type t = key_hash [@@deriving eq, ord, yojson]

    (* repr *)
    val of_b58 : string -> key_hash option
    val to_b58 : key_hash -> string

    (* operations *)
    val of_key : Key.t -> key_hash

    (* We need this to parse the key hashes
       returned by Taquito. We eventually want to
       move this stuff to its own library or something.
       FIXME: can we get rid of this? *)
    val encoding : key_hash Data_encoding.t
  end

  module Signature : sig
    type signature
    type t = signature [@@deriving eq, ord, yojson]

    (* repr *)
    val of_b58 : string -> signature option
    val to_b58 : signature -> string

    (* utils *)
    val zero : signature
    val size : int

    (* operations *)
    val sign : ?prefix:string -> Secret.t -> BLAKE2b.t -> signature
    val verify : ?prefix:string -> Key.t -> signature -> BLAKE2b.t -> bool
  end
end
