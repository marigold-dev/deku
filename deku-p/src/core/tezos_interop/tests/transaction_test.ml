open Deku_protocol
open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Common

let main ticket_id receiver secret verbose host =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let url = Uri.with_path host "api/v1/chain/level" in
  let level =
    let response = Net.get ~sw ~env url in
    let body = Net.body_of_response response in
    if verbose then prerr_endline body;
    let level = Net.level_body_of_yojson body in
    Level.next (Level.next level)
  in
  let identity = Identity.make secret in
  let nonce = Nonce.of_n (Obj.magic level) in
  let operation =
    Operation.Signed.ticket_transfer ~identity ~level ~nonce ~receiver
      ~ticket_id
      ~amount:(Deku_concepts.Amount.of_n (Obj.magic 10))
  in
  let url = Uri.with_path host "api/v1/operations" in
  let json =
    Data_encoding.Json.construct Deku_protocol.Operation.Signed.encoding
      operation
  in
  Printf.eprintf "%s\n%!" (Data_encoding.Json.to_string json);
  let response = Net.post_operation ~sw ~env operation url in
  let code = Net.code_of_response response in
  Printf.eprintf "%d\n%!" code;
  let (Signed_operation { initial = Initial_operation { hash; _ }; _ }) =
    operation
  in
  let hash = Operation_hash.to_blake2b hash in
  Printf.printf "operation.hash: %s\n%!" (BLAKE2b.to_hex hash);
  exit 0

open Cmdliner

let info =
  let doc = "Do a transaction" in
  Cmd.info "deku-transaction-test" ~version:"%\226\128\140%VERSION%%" ~doc

let term =
  let open Term in
  const main $ Common.ticket_id 0 $ Common.deku_address 1 $ Common.secret 2
  $ Common.verbose_test $ Common.host

let _ = Cmd.eval ~catch:true @@ Cmd.v info term
