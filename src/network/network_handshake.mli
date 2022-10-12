open Deku_crypto
open Deku_concepts

module Challenge : sig
  type challenge
  type t = challenge

  (* repr *)
  val encoding : challenge Data_encoding.t

  (* methods *)
  val generate : unit -> challenge
end

module Response : sig
  type response
  type t = response

  (* repr *)
  val encoding : response Data_encoding.t

  (* methods *)
  val key : response -> Key.t
  val answer : identity:Identity.t -> Challenge.t -> response
  val verify : challenge:Challenge.t -> response -> bool
end
