(* Called by a script to do a withdraw and print the hash to stdout *)
open Deku_stdlib
open Deku_protocol
open Deku_concepts
open Lwt_result.Syntax

(* Still have that Piaf bug *)
let post ~sw ~env body uri =
  let body = Piaf.Body.of_string body in
  match Piaf.Client.Oneshot.post ~body env uri ~sw with
  | Ok response -> response
  | Error error -> failwith (Piaf.Error.to_string error)

let main ticket_id tezos_owner secret verbose host =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let url = Uri.with_path host "api/v1/chain/level" in
  let level =
    Lwt_main.run
      (let* response = Piaf_lwt.Client.Oneshot.get url in
       let* level = Piaf_lwt.Body.to_string response.body in
       if verbose then prerr_endline level;
       match Yojson.Safe.from_string level with
       | `Assoc [ ("level", level) ] ->
           let level = Level.next (Level.next (Level.t_of_yojson level)) in
           Lwt.return (Ok level)
       | _ -> Lwt.return (Error `Bad_request))
    |> Result.get_ok
  in
  let identity = Identity.make secret in
  let nonce = Nonce.of_n (Obj.magic level) in
  let url = Uri.with_path host "api/v1/operations" in
  let transaction =
    Deku_protocol.Operation.withdraw ~identity ~level ~nonce ~tezos_owner
      ~ticket_id
      ~amount:(Deku_concepts.Amount.of_n (Obj.magic 10))
  in
  let json = Deku_protocol.Operation.yojson_of_t transaction in

  let () =
    let body = Yojson.Safe.to_string json in
    let response = post ~sw ~env body url in
    let status = response.Piaf.Response.status in
    Printf.eprintf "%a\n%!" code;
    Lwt.return_unit
  in
  let (Operation.Operation { hash; _ }) = transaction in
  let hash = Operation_hash.yojson_of_t hash |> Yojson.Safe.to_string in
  Printf.printf "operation.hash: %s\n%!" hash;
  if verbose then (
    Printf.eprintf "operation code: %s\n%!"
      (Operation.yojson_of_t transaction |> Yojson.Safe.to_string);
    prerr_endline "operation broadcasted")
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
