open Deku_concepts

module Network : sig
  type network = private
    | Network_request of { raw_header : string; raw_content : string }

  type t = network
end

type request = private
  | Request of { hash : Request_hash.t; above : Level.t; network : Network.t }

type t = request

exception Expected_hash_mismatch
exception Invalid_message

val encode : above:Level.t -> request
val decode : expected:Request_hash.t -> raw_content:string -> request
