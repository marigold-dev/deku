open Deku_stdlib
open Deku_concepts
open Deku_tezos_interop
open Deku_crypto
open Deku_indexer
open Deku_external_vm
include Node

type params = {
  domains : int; [@env "DEKU_DOMAINS"] [@default 8]
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
  save_blocks : bool; [@env "DEKU_DEBUG_SAVE_BLOCKS"] [@default true]
      (** Configures the debug indexer to save blocks. Defaults to true. *)
  save_messages : bool; [@env "DEKU_DEBUG_SAVE_MESSAGES"] [@default true]
      (** Configures the debug indexer to save consensus messages. Defaults to true. *)
  default_block_size : int; [@env "DEKU_DEFAULT_BLOCK_SIZE"] [@default 50_000]
      (** The threshold below which blocks are filled with no-op transactions. *)
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
  named_pipe_path : string; [@default "deku_vm"]
      (** Named pipe path to use for IPC with the VM *)
}
[@@deriving cmdliner]

let main params =
  Lwt_main.run
  @@
  let {
    domains;
    bootstrap_key;
    secret;
    validators;
    validator_uris;
    port;
    database_uri;
    save_blocks;
    save_messages;
    default_block_size;
    tezos_rpc_node;
    tezos_required_confirmations;
    tezos_secret;
    tezos_consensus_address;
    tezos_discovery_address;
    named_pipe_path;
  } =
    params
  in
  let%await indexer =
    Indexer.make ~uri:database_uri
      ~config:Indexer.{ save_blocks; save_messages }
  in
  let tezos_interop =
    Tezos_interop.make ~rpc_node:tezos_rpc_node
      ~secret:(Secret.Ed25519 tezos_secret)
      ~consensus_contract:tezos_consensus_address
      ~discovery_contract:tezos_discovery_address
      ~required_confirmations:tezos_required_confirmations
  in
  (* FIXME: we need to load the initial state when we have snapshotting.
     This involves one of two things:
       - load some initial state defined by the VM.
       - load the state from disk. *)
  (* FIXME: Also, startup order matters, otherwise you hit deadlock on the named pipes. *)
  let () = External_vm_client.start_vm_ipc ~named_pipe_path in
  let vm_state = External_vm_client.get_initial_state () in
  let identity = Identity.make (Secret.Ed25519 secret) in
  Logs.info (fun m ->
      m "Running as validator %s" (Identity.key_hash identity |> Key_hash.to_b58));
  let pool = Parallel.Pool.make ~domains in
  let node, promise =
    Node.make ~pool ~identity ~validators ~nodes:validator_uris
      ~bootstrap_key:(Key.Ed25519 bootstrap_key) ~indexer:(Some indexer)
      ~default_block_size ~vm_state
  in
  Node.listen node ~port ~tezos_interop;
  promise

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

let () = setup_log ~level:Logs.Info ()

let () =
  Logs.info (fun m -> m "Starting node");
  let info = Cmdliner.Cmd.info Sys.argv.(0) in
  let term = Cmdliner.Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmdliner.Cmd.v info term in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)
