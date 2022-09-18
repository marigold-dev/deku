open Deku_stdlib
open Deku_concepts
open Deku_crypto
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
}
[@@deriving cmdliner]

let main params =
  Lwt_main.run
  @@
  let { bootstrap_key; secret; validators; validator_uris; port } = params in
  let pool = Parallel.Pool.make ~domains in
  let identity = Identity.make (Secret.Ed25519 secret) in
  Parallel.Pool.run pool (fun () ->
      let node, promise =
        Node.make ~pool ~identity ~validators ~nodes:validator_uris
          ~bootstrap_key:(Key.Ed25519 bootstrap_key)
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
