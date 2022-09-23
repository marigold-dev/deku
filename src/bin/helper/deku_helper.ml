open Deku_protocol
open Deku_stdlib
open Deku_concepts
open Deku_gossip
open Piaf_lwt
open Deku_crypto

let post_directly_to_node ~operation =
  let node = "http://localhost:4440/messages" in
  let content = Message.Content.operation operation in
  let _message, raw_message = Message.encode ~content in
  let (Message.Raw_message { hash; raw_content }) = raw_message in
  let hash, raw_content = (hash |> Message_hash.to_b58, raw_content) in
  let headers =
    let open Headers in
    (* TODO: maybe add json to Well_known in Piaf*)
    let json = Mime_types.map_extension "json" in
    [ ("X-Raw-Expected-Hash", hash); (Well_known.content_type, json) ]
  in
  let target = Uri.of_string node in
  (* Format.eprintf "%a <- %s\n%!" Uri.pp_hum _uri target; *)
  let body = Body.of_string raw_content in

  let%await post_result = Piaf_lwt.Client.Oneshot.post ~headers ~body target in
  (match post_result with
  | Error _error -> print_endline "error"
  | Ok _ -> print_endline "submitted");
  Lwt.return_unit

let post_to_api ~operation =
  let node = "http://localhost:8080/api/v1/operations" |> Uri.of_string in
  let json = Operation.yojson_of_t operation |> Yojson.Safe.to_string in
  let body = Body.of_string json in
  let%await post_result = Piaf_lwt.Client.Oneshot.post ~body node in
  (match post_result with
  | Ok _ -> print_endline "operation submitted"
  | Error _ -> print_endline "FAIL to submit operation");
  Lwt.return_unit

let make_identity secret =
  secret |> Secret.of_b58 |> Option.get |> Identity.make

type level_response = { level : Level.t } [@@deriving of_yojson]

let make_level () =
  let%await response =
    Piaf_lwt.Client.Oneshot.get
      (Uri.of_string "http://localhost:8080/api/v1/chain/level")
  in
  let body =
    match response with
    | Error _ -> failwith "cannot connect to the API"
    | Ok res -> res.body
  in
  let%await string = Body.to_string body in
  let body =
    match string with
    | Error _ -> failwith "cannot parse body"
    | Ok body -> body
  in
  let yojson = Yojson.Safe.from_string body in
  let { level } = level_response_of_yojson yojson in
  Lwt.return level

let make_nonce () =
  let rng = Stdlib.Random.State.make_self_init () in
  Stdlib.Random.State.bits64 rng
  |> Int64.abs |> Z.of_int64 |> N.of_z |> Option.get |> Nonce.of_n

let print_op operation =
  operation |> Operation.yojson_of_t |> Yojson.Safe.to_string |> print_endline

let main () =
  let identity =
    make_identity "edsk4UWkJqpZrAm26qvJE8uY9ZFGFqQiFuBcDyEPASXeHxuD68WvvF"
  in
  let%await level = make_level () in
  let nonce = make_nonce () in

  (* Change this string with your appropriate needs*)
  let content =
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

  let%await _ = post_directly_to_node ~operation:transaction in
  Lwt.return_unit

let () = Lwt_main.run (main ())
