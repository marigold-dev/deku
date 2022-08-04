open Deku_crypto

type storage = private {
  secret : Secret.t; (* bootstrap *)
  initial_validators : Key_hash.t list;
  nodes : Uri.t list;
  bootstrap_key : Key.t;
}

type t = storage [@@deriving yojson]

val make :
  secret:Secret.t ->
  initial_validators:Key_hash.t list ->
  nodes:Uri.t list ->
  bootstrap_key:Key.t ->
  storage

val read : file:string -> storage Lwt.t
val write : file:string -> storage -> unit Lwt.t
