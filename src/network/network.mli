open Deku_concepts
open Deku_protocol
open Deku_consensus

type network = private
  | Network of { nodes : Uri.t list; known_messages : Message_hash.Set.t }

type t = network

val make : nodes:Uri.t list -> network

val incoming_message :
  endpoint:'a Endpoint.t -> message:string -> network -> 'a option * network

val broadcast_block : block:Block.t -> network -> network
val broadcast_signature : signature:Verified_signature.t -> network -> network
val broadcast_operation : operation:Operation.t -> network -> network
