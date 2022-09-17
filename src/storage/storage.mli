open Deku_crypto

type storage = private {
  secret : Secret.t; (* bootstrap *)
  validators : (Key_hash.t * Uri.t) list;
}

type t = storage [@@deriving yojson]

val make : secret:Secret.t -> validators:(Key_hash.t * Uri.t) list -> storage
val read : file:string -> storage Lwt.t
val write : file:string -> storage -> unit Lwt.t
