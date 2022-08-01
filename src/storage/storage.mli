open Deku_crypto

type storage = private {
  secret : Secret.t; (* bootstrap *)
  initial_validators : Key_hash.t list;
  nodes : Uri.t list;
}

type t = storage [@@deriving yojson]

val read : unit -> storage Lwt.t
