open Deku_protocol
open Deku_crypto
open Deku_concepts
open Lwt_result.Syntax
open Cohttp_lwt_unix
open Lwt

(*
(* FIXME this does not work, don't know why *)
let post body uri =
  let headers =
    let open Piaf.Headers in
    (* TODO: maybe add json to Well_known in Piaf*)
    let json = Mime_types.map_extension "json" in
    [ (Well_known.content_type, json) ]
  in
  let body = Piaf.Body.of_string body in
  prerr_endline "oui";
  let* post_result = Piaf.Client.Oneshot.post ~headers ~body uri in
  prerr_endline "test";  (* never shown *)
  Lwt.return (Ok post_result)
  *)

let post body uri =
  let body = Cohttp_lwt.Body.of_string body in
  Client.post ~body uri

let () =
  let ticket = "KT18h9RX4oh7YBHxdjZr8kWEW14JeZ6bybah" in
  let url = "http://localhost:4441/level" in
  let level =
    Lwt_main.run
      (let* response = Piaf.Client.Oneshot.get (Uri.of_string url) in
       let* level = Piaf.Body.to_string response.body in
       prerr_endline level;
       let level = Yojson.Safe.from_string level in
       let level = Level.next (Level.next (Level.t_of_yojson level)) in
       Lwt.return (Ok level))
    |> Result.get_ok
  in
  let secret =
    Secret.of_b58 "edsk2qXLn3PdFyb5QYGCqbK5kKZFpfTmTtiUDKSA8RuKN34U3BUKHo"
    |> Option.get
  in
  let identity = Identity.make secret in
  let source =
    let key_hash = Identity.key_hash identity in
    Address.of_key_hash key_hash
  in
  let receiver =
    Address.of_b58 "tz1bPFuX14fYT43fnmXbxzZuyT7ABJic7Rz7" |> Option.get
  in
  let ticketer = Deku_tezos.Contract_hash.of_string ticket |> Option.get in
  let data = Bytes.of_string "" in
  let ticket_id = Ticket_id.make ticketer data in
  let nonce = Nonce.of_n (Obj.magic level) in
  let operation =
    Operation.transaction ~identity ~level ~nonce ~source ~receiver ~ticket_id
      ~amount:(Deku_concepts.Amount.of_int 10 |> Option.get)
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
  Printf.printf "operation.hash: %s\n%!" (BLAKE2b.to_hex hash);
  Printf.printf "operation code: %s\n%!"
    (Operation.yojson_of_t operation |> Yojson.Safe.to_string);
  prerr_endline "operation broadcasted";
  ()
