open Deku_crypto
open Deku_concepts
open Deku_protocol
open Deku_stdlib
open Cmdliner
open Common

type params = {
  secret : Ed25519.Secret.t;
  api_uri : Uri.t; [@default Uri.of_string "http://localhost:8080"]
  content : string; [@pos 0]
}
[@@deriving cmdliner]

(* Submits a parametric operation to the chain*)
let main { secret; api_uri; content } =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let () = Stdlib.Random.self_init () in
  let secret = Secret.Ed25519 secret in
  let identity = Identity.make secret in
  let nonce = Utils.make_rnd_nonce () in
  let level = Api.current_level ~sw ~env ~api_uri in
  let operation = Operation.vm_transaction ~level ~nonce ~content ~identity in
  let (Operation.Operation { hash; _ }) = operation in
  let () = Api.submit_operation ~sw ~env ~operation ~api_uri in
  print_endline (Operation_hash.to_b58 hash);
  exit 0

let cmd =
  let term = Term.(const main $ params_cmdliner_term ()) in
  let info = Cmd.info "create-custom-transaction" in
  Cmd.v info term