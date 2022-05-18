open Crypto

type t [@@deriving yojson]

val compare : t -> t -> int

val public_key : t -> Wallet.t

val address : t -> Key_hash.t

val signature : t -> Signature.t

val sign : key:Secret.t -> BLAKE2B.t -> t

val verify : signature:t -> BLAKE2B.t -> bool

module type S = sig
  type value

  type signature = t

  type t = private {value : value; signature : signature}

  val sign : key:Secret.t -> value -> t

  val verify : signature:signature -> value -> bool
end

module Make : functor
  (P : sig
     type t

     val hash : t -> BLAKE2B.t
   end)
  -> S with type value = P.t
