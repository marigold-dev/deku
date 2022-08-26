open Broadcast
open Deku_stdlib

type network =
  | Network of { nodes : Uri.t list; known_packets : Packet_hash.Set.t }

type t = network

let make ~nodes = Network { nodes; known_packets = Packet_hash.Set.empty }

exception Duplicated_packet
exception Invalid_hash

let incoming_packet (type a) ~(endpoint : a Endpoint.t) ~packet network :
    a option * network =
  (* Trace.dump "Incoming packet"; *)
  let packet_json = Yojson.Safe.from_string packet in
  let (Packet { hash; content } as packet) = Packet.t_of_yojson packet_json in
  (* Trace.dump "Incoming packet deserialized"; *)
  let (Network { nodes; known_packets }) = network in
  match Packet_hash.Set.mem hash known_packets with
  | true -> raise Duplicated_packet
  | false -> (
      match Packet.verify packet with
      | true -> (
          let known_packets = Packet_hash.Set.add hash known_packets in
          let network = Network { nodes; known_packets } in

          match
            (* TODO: really important, how to prevent spam?
                Can the same parsed data be derived from two different hashes? *)
            (* TODO: does it make sense here? *)
            (* let () = broadcast_json ~nodes ~endpoint ~packet:packet_json in *)
            Packet.content_of_yojson ~endpoint content
          with
          | content ->
              (* Trace.dump "Incoming packet parsed"; *)
              (Some content, network)
          | exception exn ->
              (* TODO: proper logging *)
              Format.eprintf "Exception while parsing packet: %s\n%!"
                (Printexc.to_string exn);
              (None, network))
      | false -> (* TODO: spam prevention *) raise Invalid_hash)

let incoming_packet ~endpoint ~packet network =
  match incoming_packet ~endpoint ~packet network with
  | packet, network -> (packet, network)
  | exception _exn -> (* TODO: dump exception*) (None, network)

let broadcast ~endpoint ~content network =
  let (Network { nodes; known_packets }) = network in
  let packet =
    let content = Packet.yojson_of_content ~endpoint content in
    Packet.make ~content
  in
  (* TODO: this is ideal but leads to problems *)
  (* let known_packets = Packet_hash.Set.add hash known_packets in *)
  let () = broadcast_packet ~nodes ~endpoint ~packet in
  Network { nodes; known_packets }

let broadcast_block ~block network =
  broadcast ~endpoint:Endpoint.blocks ~content:block network

let broadcast_signature ~signature network =
  broadcast ~endpoint:Endpoint.signatures ~content:signature network

let broadcast_operation ~operation network =
  broadcast ~endpoint:Endpoint.operations ~content:operation network

let broadcast_bootstrap_signal ~bootstrap_signal network =
  broadcast ~endpoint:Endpoint.bootstrap ~content:bootstrap_signal network
