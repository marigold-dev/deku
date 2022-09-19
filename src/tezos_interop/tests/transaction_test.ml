open Deku_protocol
open Deku_crypto
open Deku_concepts
open Lwt_result.Syntax
open Cohttp_lwt_unix
open Lwt

let post body uri =
  let body = Cohttp_lwt.Body.of_string body in
  Client.post ~body uri

let main ticket_id receiver secret verbose =
  let url = "http://localhost:4441/level" in
  let level =
    Lwt_main.run
      (let* response = Piaf.Client.Oneshot.get (Uri.of_string url) in
       let* level = Piaf.Body.to_string response.body in
       if verbose then prerr_endline level;
       let level = Yojson.Safe.from_string level in
       let level = Level.next (Level.next (Level.t_of_yojson level)) in
       Lwt.return (Ok level))
    |> Result.get_ok
  in
  let identity = Identity.make secret in
  let source =
    let key_hash = Identity.key_hash identity in
    Address.of_key_hash key_hash
  in
  let nonce = Nonce.of_n (Obj.magic level) in
  let operation =
    Operation.transaction ~identity ~level ~nonce ~source ~receiver ~ticket_id
      ~amount:(Deku_concepts.Amount.of_n (Obj.magic 10))
  in
  let uris =
    [
      "http://localhost:4440/operations";
      (* "http://localhost:4441/";
         "http://localhost:4442/";
         "http://localhost:4443/" *)
    ]
    |> List.map Uri.of_string
  in
  let body = Operation.yojson_of_t operation in
  let packet = Deku_network.Packet.make ~content:body in
  let packet = Deku_network.Packet.yojson_of_t packet in
  Lwt_main.run
    (Lwt_list.iter_p
       (fun uri ->
         let body = Yojson.Safe.to_string packet in
         post body uri >>= fun _ -> Lwt.return_unit)
       uris);
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
  $ Common.verbose_test

let _ = Cmd.eval ~catch:true @@ Cmd.v info term
