open Deku_protocol
open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Lwt.Syntax
open Cohttp_lwt_unix

let post body uri =
  let body = Cohttp_lwt.Body.of_string body in
  Client.post ~body uri

let get uri = Client.get uri

let main ticket_id receiver secret verbose host =
  let url = Uri.with_path host "api/v1/chain/level" in
  let level =
    Lwt_main.run
      (let* response, body = get url in
       let code = response |> Response.status |> Cohttp.Code.code_of_status in
       if verbose then Printf.eprintf "Code: %d\n%!" code;
       let* level = Cohttp_lwt.Body.to_string body in
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
  let operation =
    Operation.ticket_transfer ~identity ~level ~nonce ~receiver ~ticket_id
      ~amount:(Deku_concepts.Amount.of_n (Obj.magic 10))
  in
  let url = Uri.with_path host "api/v1/operations" in
  let json = Deku_protocol.Operation.yojson_of_t operation in
  Printf.eprintf "%s\n%!" (Yojson.Safe.to_string json);
  Lwt_main.run
    (let body = Yojson.Safe.to_string json in
     let%await response, _body = post body url in
     let code = response |> Response.status |> Cohttp.Code.code_of_status in
     Printf.eprintf "%d\n%!" code;
     Lwt.return_unit);
  let (Operation.Operation { hash; _ }) = operation in
  let hash = Operation_hash.to_blake2b hash in
  Printf.printf "operation.hash: %s\n%!" (BLAKE2b.to_hex hash)

open Cmdliner

let info =
  let doc = "Do a transaction" in
  Cmd.info "deku-transaction-test" ~version:"%\226\128\140%VERSION%%" ~doc

let term =
  let open Term in
  const main $ Common.ticket_id 0 $ Common.deku_address 1 $ Common.secret 2
  $ Common.verbose_test $ Common.host

let _ = Cmd.eval ~catch:true @@ Cmd.v info term
