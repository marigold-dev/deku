open Deku_stdlib
open Deku_concepts
open Deku_consensus
open Deku_chain
open Deku_network
open Deku_tezos_interop
open Deku_protocol
open Deku_indexer
open Deku_crypto

module Parallel = struct
  let domains =
    match Sys.getenv_opt "DEKU_DOMAINS" with
    | Some str -> int_of_string str
    | None -> 8

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
      | Chain.Commit_state_hash
          {
            current_level;
            payload_hash;
            state_root_hash;
            signatures;
            validators;
            withdrawal_handles_hash;
          } ->
          let () =
            Lwt.async (fun () ->
                let%await () =
                  Tezos_interop.Consensus.commit_state_hash
                    ~block_level:current_level ~block_payload_hash:payload_hash
                    ~state_hash:state_root_hash ~withdrawal_handles_hash
                    ~signatures ~validators tezos_interop
                in
                let%await () = Lwt_unix.sleep 1. in
                Printf.eprintf "state hash committed\n%!";
                Lwt.return_unit)
          in
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
                ~current chain
          | Level | Withdraw_proof ->
              (* TODO make these cases impossible? *)
              (chain, []))
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
    tezos_interop:Tezos_interop.t ->
    indexer:Indexer.t ->
    secret:Secret.t ->
    bootstrap_key:Key.t ->
    initial_validators:Key_hash.t list ->
    validator_uris:Uri.t list ->
    unit
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

  let initialize ~tezos_interop ~indexer ~secret ~bootstrap_key
      ~initial_validators ~validator_uris =
    let identity = Identity.make secret in

    let applied_block_ref = ref (fun () -> ()) in
    let applied_block () = !applied_block_ref () in
    let node =
      Node.make ~identity ~bootstrap_key ~validators:initial_validators
        ~nodes:validator_uris ~applied_block ~tezos_interop ~indexer
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

  let level status =
    let (Node { chain; _ }) = Singleton.get_state () in
    let (Chain { consensus; _ }) = chain in
    let (Consensus { current_level; _ }) = consensus in
    let level = Level.to_n current_level |> N.to_z |> Z.to_int in
    let response = Printf.sprintf "\"%d\"" level in
    let response = Response.of_string ~body:response status in
    Lwt.return response

  let proof body status =
    let%await body = Body.to_string body in
    let body = Result.get_ok body in
    let body = Yojson.Safe.from_string body in
    let operation_hash = Operation_hash.t_of_yojson body in
    let (Node { chain = Chain { protocol; consensus; verifier; _ }; _ }) =
      Singleton.get_state ()
    in
    let (Consensus { current_block = block_hash; _ }) = consensus in
    let withdrawal_handles_hash =
      Verifier.current_withdrawal_hash ~block_hash verifier
    in
    let withdraw_proof =
      Protocol.find_withdraw_proof ~operation_hash protocol
    in
    let response =
      match (withdrawal_handles_hash, withdraw_proof) with
      | Error _, _ -> Response.of_string ~body:"Unknown error" `Not_found
      | _, Error _ -> Response.of_string ~body:"Proof not found" `Not_found
      | Ok withdrawal_handles_hash, Ok (handle, proof) ->
          let open Ledger.Proof_response in
          let body = Proof { withdrawal_handles_hash; handle; proof } in
          let response_body = yojson_of_t body |> Yojson.Safe.to_string in
          Response.of_string ~body:response_body status
    in
    Lwt.return response

  let with_endpoint Server.{ ctx = _; request } next =
    let path = request.target in
    let meth = request.meth in
    let body = request.body in

    match Endpoint.of_string path with
    | Some (Ex Withdraw_proof) -> proof body `Accepted
    | Some (Ex Level) -> level `Accepted
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

type params = {
  secret : Ed25519.Secret.t; [@env "DEKU_SECRET"]
      (** The base58-encoded secret used as the Deku-node's identity. *)
  bootstrap_key : Ed25519.Key.t; [@env "DEKU_BOOTSTRAP_KEY"]
      (** The base58-encoded public key with which to verify signed bootstrap signals. *)
  validators : Key_hash.t list; [@env "DEKU_VALIDATORS"]
      (** A comma separeted list of the key hashes of all validators in the network. *)
  validator_uris : Uri.t list; [@env "DEKU_VALIDATOR_URIS"]
      (** A comma-separated list of the validator URI's used to join the network. *)
  port : int; [@default 4440] [@env "DEKU_PORT"]  (** The port to listen on. *)
  database_uri : Uri.t; [@env "DEKU_DATABASE_URI"]
      (** A URI-encoded path to a SQLite database. Will be created it if it doesn't exist already. *)
  tezos_rpc_node : Uri.t; [@env "DEKU_TEZOS_RPC_NODE"]
      (** The URI of this validator's Tezos RPC node. *)
  tezos_required_confirmations : int;
      [@default 2] [@env "DEKU_TEZOS_REQUIRED_CONFIRMATIONS"]
      (** The number of blocks to wait before considering a Tezos block confirmed. *)
  tezos_secret : Ed25519.Secret.t; [@env "DEKU_TEZOS_SECRET"]
      (** The base58-encoded ED25519 secret to use as the wallet for submitting Tezos transactions. *)
  tezos_consensus_address : Deku_tezos.Address.t;
      [@env "DEKU_TEZOS_CONSENSUS_ADDRESS"]
      (** The address of the consensus contract on Tezos.  *)
  tezos_discovery_address : Deku_tezos.Address.t;
      [@env "DEKU_TEZOS_DISCOVERY_ADDRESS"]
      (** The address of the discovery contract on Tezos. *)
}
[@@deriving cmdliner]

let main params =
  let {
    bootstrap_key;
    secret;
    validators;
    validator_uris;
    port;
    database_uri;
    tezos_rpc_node;
    tezos_required_confirmations;
    tezos_secret;
    tezos_consensus_address;
    tezos_discovery_address;
  } =
    params
  in
  Lwt_main.run
  @@
  let bootstrap_key = Key.Ed25519 bootstrap_key in
  let secret = Secret.Ed25519 secret in
  let tezos_secret = Secret.Ed25519 tezos_secret in
  let tezos_interop =
    Tezos_interop.make ~rpc_node:tezos_rpc_node ~secret:tezos_secret
      ~consensus_contract:tezos_consensus_address
      ~discovery_contract:tezos_discovery_address
      ~required_confirmations:tezos_required_confirmations
  in
  let%await indexer = Indexer.make ~uri:database_uri in
  let () =
    Singleton.initialize ~indexer ~tezos_interop ~secret ~bootstrap_key
      ~initial_validators:validators ~validator_uris
  in
  Tezos_bridge.listen ();
  Server.start port

let () =
  let info = Cmdliner.Cmd.info Sys.argv.(0) in
  let term = Cmdliner.Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmdliner.Cmd.v info term in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)
