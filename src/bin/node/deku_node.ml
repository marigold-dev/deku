open Deku_stdlib
open Deku_concepts
open Deku_crypto
open Deku_indexer
open Deku_storage
open Deku_chain
open Deku_external_vm
open Deku_protocol

let make_dump_loop ~sw ~env ~folder ~chain =
  let chain_ref = ref chain in

  let domains = Eio.Stdenv.domain_mgr env in

  let rec loop () : unit =
    let chain = !chain_ref in
    (try Storage.Chain.write ~env ~folder chain
     with exn ->
       Format.eprintf "storage.failure: %s\n%!" (Printexc.to_string exn));
    loop ()
  in
  let dump chain = chain_ref := chain in
  ( Eio.Fiber.fork_sub ~sw ~on_error:Deku_constants.async_on_error @@ fun _sw ->
    Eio.Domain_manager.run domains (fun () -> loop ()) );
  dump

type params = {
  domains : int; [@env "DEKU_DOMAINS"] [@default 8]
  secret : Ed25519.Secret.t; [@env "DEKU_SECRET"]
      (** The base58-encoded secret used as the Deku-node's identity. *)
  data_folder : string; [@env "DEKU_DATA_FOLDER"]
      (** Folder path where node's state is stored. *)
  validators : Key_hash.t list; [@env "DEKU_VALIDATORS"]
      (** A comma separeted list of the key hashes of all validators in the network. *)
  validator_uris : (string * int) list; [@env "DEKU_VALIDATOR_URIS"]
      (** A comma-separated list of the validator URI's used to join the network. *)
  port : int; [@default 4440] [@env "DEKU_PORT"]  (** The port to listen on. *)
  database_uri : Uri.t; [@env "DEKU_DATABASE_URI"]
      (** A URI-encoded path to a SQLite database. Will be created it if it doesn't exist already. *)
  save_blocks : bool; [@env "DEKU_DEBUG_SAVE_BLOCKS"] [@default true]
      (** Configures the debug indexer to save blocks. Defaults to true. *)
  save_messages : bool; [@env "DEKU_DEBUG_SAVE_MESSAGES"] [@default true]
      (** Configures the debug indexer to save consensus messages. Defaults to true. *)
  default_block_size : int; [@env "DEKU_DEFAULT_BLOCK_SIZE"] [@default 50_000]
      (** The threshold below which blocks are filled with no-op transactions. *)
  tezos_rpc_node : Uri.t; [@env "DEKU_TEZOS_RPC_NODE"]
      (** The URI of this validator's Tezos RPC node. *)
  tezos_secret : Ed25519.Secret.t; [@env "DEKU_TEZOS_SECRET"]
      (** The base58-encoded ED25519 secret to use as the wallet for submitting Tezos transactions. *)
  tezos_consensus_address : Deku_tezos.Address.t;
      [@env "DEKU_TEZOS_CONSENSUS_ADDRESS"]
      (** The address of the consensus contract on Tezos.  *)
  tezos_discovery_address : Deku_tezos.Address.t;
      [@env "DEKU_TEZOS_DISCOVERY_ADDRESS"]
      (** The address of the discovery contract on Tezos. *)
  named_pipe_path : string; [@default "deku_vm"]
      (** Named pipe path to use for IPC with the VM *)
  api_enabled : bool; [@env "DEKU_API_ENABLED"]
  api_port : int; [@default 8080] [@env "DEKU_API_PORT"]
}
[@@deriving cmdliner]

let main params =
  let {
    domains;
    secret;
    data_folder;
    validators;
    validator_uris;
    port;
    database_uri;
    save_blocks;
    save_messages;
    default_block_size;
    tezos_rpc_node;
    tezos_secret;
    tezos_consensus_address;
    tezos_discovery_address = _;
    named_pipe_path;
    api_enabled = _;
    api_port = _;
  } =
    params
  in
  Logs.info (fun m -> m "Default block size: %d" default_block_size);
  let indexer =
    Indexer.make ~uri:database_uri
      ~config:Indexer.{ save_blocks; save_messages }
  in
  let pool = Parallel.Pool.make ~domains in
  (* The VM must be started before the node because this call is blocking  *)
  let () = External_vm_client.start_vm_ipc ~named_pipe_path in
  let identity = Identity.make (Secret.Ed25519 secret) in
  Logs.info (fun m ->
      m "Running as validator %s" (Identity.key_hash identity |> Key_hash.to_b58));

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Parallel.Pool.run pool @@ fun () ->
  (* TODO: one problem of loading from disk like this, is that there
       may be pending actions such as fragments being processed *)
  let chain = Storage.Chain.read ~env ~folder:data_folder in
  let chain =
    match chain with
    | Some chain ->
        let (Chain { protocol; _ }) = chain in
        let (Protocol.Protocol { vm_state; _ }) = protocol in
        External_vm_client.set_initial_state vm_state;
        Chain.clear chain
    | None ->
        let vm_state = External_vm_client.get_initial_state () in
        Chain.make ~validators ~vm_state
  in
  let dump = make_dump_loop ~sw ~env ~folder:data_folder ~chain in
  let notify_api _ = () in
  let node =
    Node.make ~identity ~default_block_size ~pool ~dump ~chain
      ~indexer:(Some indexer) ~notify_api
  in

  let (Chain { consensus; _ }) = chain in
  let (Block { level; _ }) = Deku_consensus.Consensus.trusted_block consensus in
  Format.eprintf "Chain started at level: %a\n%!" Level.pp level;
  let tezos =
    (tezos_rpc_node, Secret.Ed25519 tezos_secret, tezos_consensus_address)
  in
  Node.start ~sw ~env ~port ~nodes:validator_uris ~tezos:(Some tezos) node

let setup_log ?style_renderer ?level () =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  (* disable all non-deku logs *)
  List.iter
    (fun src ->
      let src_name = Logs.Src.name src in
      if
        (not (String.starts_with ~prefix:"deku" src_name))
        && not (String.equal src_name "application")
      then Logs.Src.set_level src (Some Logs.Error))
    (Logs.Src.list ())

let () = setup_log ~level:Logs.Debug ()

(* let main () = Node.test () *)
let () =
  Sys.set_signal Sys.sigpipe
    (Sys.Signal_handle (fun _ -> Format.eprintf "SIGPIPE\n%!"))

let () =
  Logs.info (fun m -> m "Starting node");
  let info = Cmdliner.Cmd.info Sys.argv.(0) in
  let term = Cmdliner.Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmdliner.Cmd.v info term in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)
