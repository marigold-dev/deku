open Deku_protocol
open Deku_consensus
open Deku_network

module Node = struct
  type node =
    | Node of {
        protocol : Protocol.t;
        consensus : Consensus.t;
        verifier : Verifier.t;
        signer : Signer.t;
        producer : Producer.t;
        network : Network.t;
      }

  type t = node

  let apply_block ~current ~block node =
    let (Node { protocol; consensus; verifier; signer; producer; network }) =
      node
    in
    let consensus = Consensus.apply_block ~current ~block consensus in
    let protocol, receipts =
      let (Block.Block { level; payload; _ }) = block in
      Protocol.apply ~parallel:List.filter_map ~current_level:level ~payload
        protocol
    in
    let producer = Producer.clean ~receipts producer in
    let network =
      match Producer.try_to_produce ~current ~consensus producer with
      | Some block -> Network.broadcast_block ~block network
      | None -> network
    in
    Node { protocol; consensus; verifier; signer; producer; network }

  let incoming_block ~current ~block node =
    let (Node { protocol; consensus; verifier; signer; producer; network }) =
      node
    in
    let Verifier.{ apply; verifier } =
      Verifier.incoming_block ~consensus ~block verifier
    in
    let network =
      match Signer.try_to_sign ~current ~consensus ~block signer with
      | Some signature -> Network.broadcast_signature ~signature network
      | None -> network
    in
    let node =
      Node { protocol; consensus; verifier; signer; producer; network }
    in
    match apply with
    | Some block -> apply_block ~current ~block node
    | None -> node

  let incoming_signature ~current ~signature node =
    let (Node { protocol; consensus; verifier; signer; producer; network }) =
      node
    in
    let Verifier.{ apply; verifier } =
      Verifier.incoming_signature ~consensus ~signature verifier
    in
    let node =
      Node { protocol; consensus; verifier; signer; producer; network }
    in

    match apply with
    | Some block -> apply_block ~current ~block node
    | None -> node

  let incoming_operation ~operation node =
    let (Node { protocol; consensus; verifier; signer; producer; network }) =
      node
    in
    let producer = Producer.incoming_operation ~operation producer in
    Node { protocol; consensus; verifier; signer; producer; network }

  let incoming_packet (type a) ~current ~(endpoint : a Endpoint.t) ~packet node
      =
    let (Node { protocol; consensus; verifier; signer; producer; network }) =
      node
    in
    let packet, network = Network.incoming_packet ~endpoint ~packet network in
    let node =
      Node { protocol; consensus; verifier; signer; producer; network }
    in
    match packet with
    | Some packet -> (
        match endpoint with
        | Blocks -> incoming_block ~current ~block:packet node
        | Signatures -> incoming_signature ~current ~signature:packet node
        | Operations -> incoming_operation ~operation:packet node)
    | None -> node
end

module State : sig
  val get_state : unit -> Node.t
  val set_state : Node.t -> unit
end = struct
  let state = ref None

  let get_state () =
    match !state with
    | Some state -> state
    | None -> failwith "uninitialized state"

  let set_state new_state = state := Some new_state
end

open Piaf

let internal_error error =
  let response = Response.or_internal_error (Error error) in
  Lwt.return response

let error ~message status =
  let response = Response.of_string ~body:message status in
  Lwt.return response

let with_endpoint Server.{ ctx = _; request } next =
  let path = request.target in
  let meth = request.meth in

  match Endpoint.of_string path with
  | Some endpoint -> (
      match meth with
      | `POST -> next Server.{ ctx = endpoint; request }
      | _ -> error ~message:"only POST is supported" `Method_not_allowed)
  | None -> error ~message:"unknown endpoint" `Not_found

let with_body Server.{ ctx = endpoint; request } next =
  let open Lwt.Infix in
  let body = request.body in
  Body.to_string body >>= fun result ->
  match result with
  | Ok body -> next Server.{ ctx = (endpoint, body); request }
  | Error error -> internal_error error

let apply Server.{ ctx = endpoint, packet; request = _ } =
  let node = State.get_state () in
  let current = Timestamp.of_float (Unix.gettimeofday ()) in
  let (Endpoint.Ex endpoint) = endpoint in
  let node = Node.incoming_packet ~current ~endpoint ~packet node in
  let () = State.set_state node in
  let response = Piaf.Response.of_string ~body:"OK" `OK in
  Lwt.return response

let handler context =
  with_endpoint context @@ fun context ->
  with_body context @@ fun context -> apply context

let main port =
  let open Lwt.Infix in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
      (* TODO: piaf error_handler *)
      Lwt_io.establish_server_with_client_socket listen_address
        (Server.create ?config:None ?error_handler:None handler)
      >|= fun _server -> Printf.printf "Listening on port %i\n%!" port);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let () =
  let port = ref 8080 in
  Arg.parse
    [ ("-p", Arg.Set_int port, " Listening port number (8080 by default)") ]
    ignore "Handle Deku communication. Runs forever.";
  main !port
