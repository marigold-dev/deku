open Deku_stdlib
open Deku_concepts
open Deku_consensus
open Deku_chain
open Deku_network
open Deku_storage

module Parallel = struct
  let domains = 8

  let pool =
    let pool = lazy (Parallel.Pool.make ~domains) in
    fun () -> Lazy.force pool
end

module Node = struct
  type node =
    | Node of {
        chain : Chain.t;
        network : Network.t;
        (* TODO: weird, but for timeouts*)
        applied_block : unit -> unit;
      }

  type t = node

  let make ~identity ~bootstrap_key ~validators ~nodes ~applied_block =
    let chain = Chain.make ~identity ~bootstrap_key ~validators in
    let network = Network.make ~nodes in
    Node { chain; network; applied_block }

  let dispatch_effect effect node =
    let (Node { chain; network; applied_block }) = node in
    let network =
      match effect with
      | Chain.Reset_timeout ->
          let () = applied_block () in
          network
      | Chain.Broadcast_block block -> Network.broadcast_block ~block network
      | Chain.Broadcast_signature signature ->
          Network.broadcast_signature ~signature network
    in
    Node { chain; network; applied_block }

  let dispatch_effects effects node =
    List.fold_left (fun node effect -> dispatch_effect effect node) node effects

  let incoming_packet (type a) ~current ~(endpoint : a Endpoint.t) ~packet node
      =
    let (Node { chain; network; applied_block }) = node in
    let packet, network = Network.incoming_packet ~endpoint ~packet network in
    let chain, effects =
      match packet with
      | Some packet -> (
          let pool = Parallel.pool () in
          match endpoint with
          | Blocks -> Chain.incoming_block ~pool ~current ~block:packet chain
          | Signatures ->
              Chain.incoming_signature ~pool ~current ~signature:packet chain
          | Operations ->
              let chain = Chain.incoming_operation ~operation:packet chain in
              (chain, [])
          | Bootstrap ->
              Chain.incoming_bootstrap_signal ~bootstrap_signal:packet ~current
                chain)
      | None -> (chain, [])
    in
    let node = Node { chain; network; applied_block } in
    dispatch_effects effects node

  let incoming_timeout ~current node =
    let (Node { chain; network; applied_block }) = node in
    let chain, effects = Chain.incoming_timeout ~current chain in
    let node = Node { chain; network; applied_block } in
    dispatch_effects effects node
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
    let Storage.{ secret; initial_validators; nodes; bootstrap_key } =
      storage
    in
    let identity = Identity.make secret in

    let applied_block_ref = ref (fun () -> ()) in
    let applied_block () = !applied_block_ref () in
    let node =
      Node.make ~identity ~bootstrap_key ~validators:initial_validators ~nodes
        ~applied_block
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
    (* TODO: weird usage of @@ *)
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
