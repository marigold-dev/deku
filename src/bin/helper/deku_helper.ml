open Deku_protocol
open Deku_stdlib
open Deku_concepts
open Deku_gossip
open Piaf_lwt
open Deku_crypto

let node = "http://localhost:4440/messages"

let post ~raw_expected_hash ~raw_content =
  let headers =
    let open Headers in
    (* TODO: maybe add json to Well_known in Piaf*)
    let json = Mime_types.map_extension "json" in
    [
      ("X-Raw-Expected-Hash", raw_expected_hash); (Well_known.content_type, json);
    ]
  in
  let target = Uri.of_string node in
  (* Format.eprintf "%a <- %s\n%!" Uri.pp_hum _uri target; *)
  let body = Body.of_string raw_content in

  let%await post_result = Piaf_lwt.Client.Oneshot.post ~headers ~body target in
  (match post_result with
  | Error _error -> print_endline "error"
  | Ok _ -> print_endline "submitted");
  Lwt.return_unit

let make_identity secret =
  secret |> Secret.of_b58 |> Option.get |> Identity.make

let make_level () = Level.zero
let make_content string = Yojson.Safe.from_string string

let make_nonce () =
  let rng = Stdlib.Random.State.make_self_init () in
  Stdlib.Random.State.bits64 rng
  |> Int64.abs |> Z.of_int64 |> N.of_z |> Option.get |> Nonce.of_n

let print_op operation =
  operation |> Operation.yojson_of_t |> Yojson.Safe.to_string |> print_endline

let make_message operation =
  let content = Message.Content.operation operation in
  let _message, raw_message = Message.encode ~content in
  let (Message.Raw_message { hash; raw_content }) = raw_message in
  (hash |> Message_hash.to_b58, raw_content)

let main () =
  let identity =
    make_identity "edsk4UWkJqpZrAm26qvJE8uY9ZFGFqQiFuBcDyEPASXeHxuD68WvvF"
  in
  let _source = Identity.key_hash identity in
  let level = make_level () in
  let nonce = make_nonce () in
  let content =
    make_content
      {|
       {
         "type" : "mint",
         "operation" : "cookie"
       }
     |}
  in

  let transaction = Operation.vm_transaction ~level ~nonce ~content ~identity in
  print_op transaction;
  print_newline ();

  let hash, raw_content = make_message transaction in
  print_endline hash;
  print_endline raw_content;

  let%await _ = post ~raw_expected_hash:hash ~raw_content in
  Lwt.return_unit

let () = Lwt_main.run (main ())
