open Deku_stdlib
open Deku_concepts
open Deku_consensus
open Deku_chain
open Deku_network
open Deku_storage
open Deku_tezos_interop
open Deku_protocol
open Deku_indexer

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
        indexer : Indexer.t;
        (* TODO: weird, but for timeouts*)
        applied_block : unit -> unit;
        tezos_interop : Tezos_interop.t;
      }

  type t = node

  let make ~identity ~bootstrap_key ~validators ~nodes ~applied_block
      ~tezos_interop ~indexer =
    let chain = Chain.make ~identity ~bootstrap_key ~validators in
    let network = Network.make ~nodes in
    Node { chain; network; applied_block; tezos_interop; indexer }

  let dispatch_effect effect node =
    let (Node { chain; network; applied_block; tezos_interop; indexer }) =
      node
    in
    let network =
      match effect with
      | Chain.Reset_timeout ->
          let () = applied_block () in
          network
      | Chain.Broadcast_block block -> Network.broadcast_block ~block network
      | Chain.Broadcast_signature signature ->
          Network.broadcast_signature ~signature network
      | Chain.Save_block block ->
          let current = Timestamp.of_float (Unix.gettimeofday ()) in
          let () = Indexer.save_block ~block ~timestamp:current indexer in
          network
    in
    Node { chain; network; applied_block; tezos_interop; indexer }

  let dispatch_effects effects node =
    List.fold_left (fun node effect -> dispatch_effect effect node) node effects

  let incoming_packet (type a) ~current ~(endpoint : a Endpoint.t) ~packet node
      =
    let (Node { chain; network; applied_block; tezos_interop; indexer }) =
      node
    in
    let () = Indexer.save_packet ~packet ~timestamp:current indexer in
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
              Chain.incoming_bootstrap_signal ~pool ~bootstrap_signal:packet
                ~current chain)
      | None -> (chain, [])
    in
    let node = Node { chain; network; applied_block; tezos_interop; indexer } in
    dispatch_effects effects node

  let incoming_timeout ~current node =
    let (Node { chain; network; applied_block; tezos_interop; indexer }) =
      node
    in
    let pool = Parallel.pool () in
    let chain, effects = Chain.incoming_timeout ~pool ~current chain in
    let node = Node { chain; network; applied_block; tezos_interop; indexer } in
    dispatch_effects effects node
end

module Singleton : sig
  val get_state : unit -> Node.t
  val set_state : Node.t -> unit

  val initialize :
    tezos_interop:Tezos_interop.t -> indexer:Indexer.t -> Storage.t -> unit
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

  let initialize ~tezos_interop ~indexer storage =
    let Storage.{ secret; initial_validators; nodes; bootstrap_key } =
      storage
    in
    let identity = Identity.make secret in

    let applied_block_ref = ref (fun () -> ()) in
    let applied_block () = !applied_block_ref () in
    let node =
      Node.make ~identity ~bootstrap_key ~validators:initial_validators ~nodes
        ~applied_block ~tezos_interop ~indexer
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

module Tezos_bridge = struct
  (* TODO: declare this function elsewhere ? *)
  let to_tezos_operation transaction =
    let open Tezos_interop.Consensus in
    match transaction with
    | Deposit { ticket; amount; destination } ->
        N.of_z amount |> Option.map Amount.of_n
        |> Option.map (fun amount ->
               Tezos_operation.Deposit { ticket; amount; destination })
    | _ -> None

  let listen () =
    let node = Singleton.get_state () in
    let (Node { tezos_interop; _ }) = node in
    Tezos_interop.Consensus.listen_operations tezos_interop
      ~on_operation:(fun operation ->
        let node = Singleton.get_state () in
        let (Node { chain; network; applied_block; tezos_interop; indexer }) =
          node
        in
        let Tezos_interop.Consensus.{ hash; transactions } = operation in
        let operations = List.filter_map to_tezos_operation transactions in
        let tezos_operation = Tezos_operation.make hash operations in
        let chain, effects =
          Chain.incoming_tezos_operation ~tezos_operation chain
        in
        let node =
          Node.Node { chain; network; applied_block; tezos_interop; indexer }
        in
        let node = Node.dispatch_effects effects node in
        Singleton.set_state node)
end

let main () =
  let port = ref 8080 in
  let storage = ref "storage.json" in
  let rpc_node = ref "http://localhost:20000" in
  let required_confirmations = ref 2 in
  let database_uri = ref "sqlite3:database.db" in
  Arg.parse
    [
      ("-p", Arg.Set_int port, " Listening port number (8080 by default)");
      ("-s", Arg.Set_string storage, " Storage file (storage.json by default)");
      ( "--rpc-node",
        Arg.Set_string rpc_node,
        " Tezos rpc node (http://localhost:20000 by default)" );
      ( "--required-confirmations",
        Arg.Set_int required_confirmations,
        " The number of required confirmations (2 by default)" );
      ( "-d",
        Arg.Set_string database_uri,
        " Database URI (sqlite3:database.db by default)" );
    ]
    ignore "Handle Deku communication. Runs forever.";

  let consensus, discovery, tezos_secret =
    match
      ( Sys.getenv_opt "DEKU_CONSENSUS_CONTRACT",
        Sys.getenv_opt "DEKU_DISCOVERY_CONTRACT",
        Sys.getenv_opt "DEKU_TEZOS_SECRET" )
    with
    | Some consensus, Some discovery, Some tezos_secret ->
        (consensus, discovery, tezos_secret)
    | None, _, _ -> failwith "consensus address is required"
    | _, None, _ -> failwith "discovery address is required"
    | _, _, None -> failwith "tezos_secret is required"
  in
  let%await storage = Storage.read ~file:!storage in
  let tezos_interop =
    Tezos_interop.make ~rpc_node:(Uri.of_string !rpc_node)
      ~secret:(tezos_secret |> Deku_crypto.Secret.of_b58 |> Option.get)
      ~consensus_contract:(Deku_tezos.Address.of_string consensus |> Option.get)
      ~discovery_contract:(Deku_tezos.Address.of_string discovery |> Option.get)
      ~required_confirmations:!required_confirmations
  in
  let database_uri = Uri.of_string !database_uri in
  let%await indexer = Indexer.make ~uri:database_uri in
  let () = Singleton.initialize ~indexer ~tezos_interop storage in
  Tezos_bridge.listen ();
  Server.start !port

let () = Lwt_main.run (main ())
