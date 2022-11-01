open Deku_concepts
open Deku_protocol
open Deku_stdlib
open Cmdliner
open Common

type params = {
  wallet : string; [@pos 0] [@docv "wallet"]
  api_uri : Uri.t; [@default Uri.of_string "http://localhost:8080"]
  content : string; [@pos 1] [@docv "transaction"]
}
[@@deriving cmdliner]

(* Submits a parametric operation to the chain*)
let main { wallet; api_uri; content } =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let () = Stdlib.Random.self_init () in
  let secret = Wallet.read ~env ~file:wallet |> Wallet.priv_key in
  let identity = Identity.make secret in
  let nonce = Utils.make_rnd_nonce () in
  let level = Api.current_level ~sw ~env ~api_uri in
  let operation = Yojson.Safe.from_string content in
  let operation = Ocaml_wasm_vm.Operation.t_of_yojson operation in
  let operation =
    Operation.Signed.vm_transaction ~level ~nonce ~content:operation ~identity
  in
  let (Signed_operation { initial = Initial_operation { hash; _ }; _ }) =
    operation
  in
  let () = Api.submit_operation ~sw ~env ~operation ~api_uri in
  print_endline (Operation_hash.to_b58 hash);
  exit 0

let cmd =
  let term = Term.(const main $ params_cmdliner_term ()) in
  let info = Cmd.info "submit-transaction" in
  Cmd.v info term
