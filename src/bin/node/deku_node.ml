open Deku_stdlib
open Deku_concepts
open Deku_crypto
open Deku_indexer
include Node

let domains = 8

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
  save_blocks : bool; [@env "DEKU_DEBUG_SAVE_BLOCKS"] [@default true]
      (** Configures the debug indexer to save blocks. Defaults to true. *)
  save_messages : bool; [@env "DEKU_DEBUG_SAVE_MESSAGES"] [@default true]
      (** Configures the debug indexer to save consensus messages. Defaults to true. *)
  default_block_size : int; [@env "DEKU_DEFAULT_BLOCK_SIZE"] [@default 50_000]
      (** The threshold below which blocks are filled with no-op transactions. *)
}
[@@deriving cmdliner]

let main params =
  Lwt_main.run
  @@
  let {
    bootstrap_key;
    secret;
    validators;
    validator_uris;
    port;
    database_uri;
    save_blocks;
    save_messages;
    default_block_size;
  } =
    params
  in
  let%await indexer =
    Indexer.make ~uri:database_uri
      ~config:Indexer.{ save_blocks; save_messages }
  in
  let identity = Identity.make (Secret.Ed25519 secret) in
  let pool = Parallel.Pool.make ~domains in
  Parallel.Pool.run pool (fun () ->
      let node, promise =
        Node.make ~pool ~identity ~validators ~nodes:validator_uris
          ~bootstrap_key:(Key.Ed25519 bootstrap_key) ~indexer
          ~default_block_size
      in
      Node.listen node ~port;
      promise)

(* let setup_log ?style_renderer level =
     Fmt_tty.setup_std_outputs ?style_renderer ();
     Logs.set_level (Some level);
     Logs.set_reporter (Logs_fmt.reporter ())

   let () = setup_log Logs.Debug *)

let () =
  let info = Cmdliner.Cmd.info Sys.argv.(0) in
  let term = Cmdliner.Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmdliner.Cmd.v info term in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)
