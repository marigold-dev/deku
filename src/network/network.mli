open Deku_concepts
open Deku_protocol
open Deku_consensus

type network = private
  | Network of {
      nodes : Uri.t list;
      known_packets : Packet_hash.Set.t;
      api : Uri.t option;
    }

type t = network

val make : nodes:Uri.t list -> api:Uri.t option -> network

val incoming_packet :
  endpoint:'a Endpoint.t -> packet:string -> network -> 'a option * network

val broadcast_block : block:Block.t -> network -> network
val broadcast_signature : signature:Verified_signature.t -> network -> network
val broadcast_operation : operation:Operation.t -> network -> network

val broadcast_bootstrap_signal :
  bootstrap_signal:Bootstrap_signal.t -> network -> network
