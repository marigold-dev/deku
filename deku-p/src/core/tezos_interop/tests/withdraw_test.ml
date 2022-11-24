(* Called by a script to do a withdraw and print the hash to stdout *)
open Deku_stdlib
open Deku_protocol
open Deku_concepts
open Common

let main ticket_id tezos_owner secret verbose host =
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
  let url = Uri.with_path host "api/v1/operations" in
  let transaction =
    Deku_protocol.Operation.Signed.withdraw ~identity ~level ~nonce ~tezos_owner
      ~ticket_id
      ~amount:(Deku_concepts.Amount.of_n (Obj.magic 10))
  in
  let () =
    let response = Net.post_operation ~sw ~env transaction url in
    let status = Net.code_of_response response in
    Printf.eprintf "%i\n%!" status
  in
  let (Signed_operation { initial = Initial_operation { hash; _ }; _ }) =
    transaction
  in
  let hash =
    Data_encoding.Json.construct Operation_hash.encoding hash
    |> Data_encoding.Json.to_string
  in
  Printf.printf "operation.hash: %s\n%!" hash;
  if verbose then (
    Printf.eprintf "operation code: %s\n%!"
      (Data_encoding.Json.construct Operation.Signed.encoding transaction
      |> Data_encoding.Json.to_string);
    prerr_endline "operation broadcasted");
  exit 0
(*  we can't test that the withdraws succeeded at this point *)

open Cmdliner

let info =
  let doc = "Do a withdraw" in
  Cmd.info "deku-withdraw-test" ~version:"%\226\128\140%VERSION%%" ~doc

let term =
  let open Term in
  const main $ Common.ticket_id 0 $ Common.tezos_address 1 $ Common.secret 2
  $ Common.verbose_test $ Common.host

let _ = Cmd.eval ~catch:true @@ Cmd.v info term
