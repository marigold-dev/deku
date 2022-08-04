open Deku_stdlib
open Deku_concepts
open Deku_protocol
open Deku_consensus
open Deku_network
open Deku_storage

module Node = struct
  type node =
    | Node of {
        protocol : Protocol.t;
        consensus : Consensus.t;
        verifier : Verifier.t;
        signer : Signer.t;
        producer : Producer.t;
        network : Network.t;
        (* TODO: weird, but for timeouts*)
        applied_block : unit -> unit;
      }

  type t = node

  let make ~identity ~validators ~nodes ~applied_block =
    let validators = Validators.of_key_hash_list validators in
    let protocol = Protocol.initial in
    let consensus = Consensus.make ~validators in
    let verifier = Verifier.empty in
    let signer = Signer.make ~identity in
    let producer = Producer.make ~identity in
    let network = Network.make ~nodes in
    Node
      {
        protocol;
        consensus;
        verifier;
        signer;
        producer;
        network;
        applied_block;
      }

  let apply_block ~current ~block node =
    let () =
      let (Block.Block { level; _ }) = block in
      let level = Level.to_n level in
      let level = N.to_z level in
      Format.eprintf "%a\n%!" Z.pp_print level
    in
    let (Node
          {
            protocol;
            consensus;
            verifier;
            signer;
            producer;
            network;
            applied_block;
          }) =
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
    (* TODO: I really don't like this *)
    let () = applied_block () in
    Node
      {
        protocol;
        consensus;
        verifier;
        signer;
        producer;
        network;
        applied_block;
      }

  let incoming_block ~current ~block node =
    let (Node
          {
            protocol;
            consensus;
            verifier;
            signer;
            producer;
            network;
            applied_block;
          }) =
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
      Node
        {
          protocol;
          consensus;
          verifier;
          signer;
          producer;
          network;
          applied_block;
        }
    in
    match apply with
    | Some block -> apply_block ~current ~block node
    | None -> node

  let incoming_signature ~current ~signature node =
    let (Node
          {
            protocol;
            consensus;
            verifier;
            signer;
            producer;
            network;
            applied_block;
          }) =
      node
    in
    let Verifier.{ apply; verifier } =
      Verifier.incoming_signature ~consensus ~signature verifier
    in
    let node =
      Node
        {
          protocol;
          consensus;
          verifier;
          signer;
          producer;
          network;
          applied_block;
        }
    in

    match apply with
    | Some block -> apply_block ~current ~block node
    | None -> node

  let incoming_operation ~operation node =
    let (Node
          {
            protocol;
            consensus;
            verifier;
            signer;
            producer;
            network;
            applied_block;
          }) =
      node
    in
    let producer = Producer.incoming_operation ~operation producer in
    Node
      {
        protocol;
        consensus;
        verifier;
        signer;
        producer;
        network;
        applied_block;
      }

  let incoming_packet (type a) ~current ~(endpoint : a Endpoint.t) ~packet node
      =
    let (Node
          {
            protocol;
            consensus;
            verifier;
            signer;
            producer;
            network;
            applied_block;
          }) =
      node
    in
    let packet, network = Network.incoming_packet ~endpoint ~packet network in
    let node =
      Node
        {
          protocol;
          consensus;
          verifier;
          signer;
          producer;
          network;
          applied_block;
        }
    in
    match packet with
    | Some packet -> (
        match endpoint with
        | Blocks -> incoming_block ~current ~block:packet node
        | Signatures -> incoming_signature ~current ~signature:packet node
        | Operations -> incoming_operation ~operation:packet node)
    | None -> node

  let incoming_timeout ~current node =
    let () = Format.printf "timeout\n%!" in
    let (Node
          {
            protocol;
            consensus;
            verifier;
            signer;
            producer;
            network;
            applied_block;
          }) =
      node
    in
    let network =
      match Producer.try_to_produce ~current ~consensus producer with
      | Some block -> Network.broadcast_block ~block network
      | None -> network
    in
    Node
      {
        protocol;
        consensus;
        verifier;
        signer;
        producer;
        network;
        applied_block;
      }
end

module Singleton : sig
  val get_state : unit -> Node.t
  val set_state : Node.t -> unit
  val initialize : Storage.t -> unit
end = struct
  type state = { mutable state : Node.t; mutable timeout : unit Lwt.t }

  let state = ref None

  let get_server () =
    match !state with
    | Some server -> server
    | None -> failwith "uninitialized state"

  let get_state () =
    let server = get_server () in
    server.state

  let set_state new_state =
    let server = get_server () in
    server.state <- new_state

  let rec reset_timeout server =
    Lwt.cancel server.timeout;
    server.timeout <-
      (let open Deku_constants in
      let%await () = Lwt_unix.sleep block_timeout in
      let current = Timestamp.of_float (Unix.gettimeofday ()) in
      server.state <- Node.incoming_timeout ~current server.state;
      reset_timeout server;
      Lwt.return_unit)

  let initialize storage =
    let Storage.{ secret; initial_validators; nodes } = storage in
    let identity = Identity.make secret in

    let applied_block_ref = ref (fun () -> ()) in
    let applied_block () = !applied_block_ref () in
    let node =
      Node.make ~identity ~validators:initial_validators ~nodes ~applied_block
    in
    let server = { state = node; timeout = Lwt.return_unit } in
    let () = applied_block_ref := fun () -> reset_timeout server in
    let () = reset_timeout server in
    state := Some server
end

module Server = struct
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
    let node = Singleton.get_state () in
    let current = Timestamp.of_float (Unix.gettimeofday ()) in
    let (Endpoint.Ex endpoint) = endpoint in
    let node = Node.incoming_packet ~current ~endpoint ~packet node in
    let () = Singleton.set_state node in
    let response = Piaf.Response.of_string ~body:"OK" `OK in
    Lwt.return response

  let handler context =
    with_endpoint context @@ fun context ->
    with_body context @@ fun context -> apply context

  let start port =
    let open Lwt.Infix in
    let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
    Lwt.async (fun () ->
        (* TODO: piaf error_handler *)
        Lwt_io.establish_server_with_client_socket listen_address
          (Server.create ?config:None ?error_handler:None handler)
        >|= fun _server -> Printf.printf "Listening on port %i\n%!" port);
    let forever, _ = Lwt.wait () in
    forever
end

let main () =
  let port = ref 8080 in
  let storage = ref "storage.json" in
  Arg.parse
    [
      ("-p", Arg.Set_int port, " Listening port number (8080 by default)");
      ("-s", Arg.Set_string storage, " Storage file (storage.json by default)");
    ]
    ignore "Handle Deku communication. Runs forever.";

  let open Lwt.Infix in
  Storage.read ~file:!storage >>= fun storage ->
  let () = Singleton.initialize storage in
  Server.start !port

let () = Lwt_main.run (main ())
