open Deku_protocol
open Deku_stdlib
open Deku_concepts
open Deku_gossip
open Deku_crypto

let post_directly_to_node ~env ~operation =
  let host = "127.0.0.1" in
  let port = 4440 in
  let net = Eio.Stdenv.net env in
  let content = Message.Content.operation operation in
  let (Message { network; _ }) = Message.encode ~content in
  let (Network_message { raw_header; raw_content }) = network in
  let open Deku_network in
  let message = Network_message.message ~raw_header ~raw_content in
  Network_protocol.Client.connect ~net ~host ~port @@ fun connection ->
  Network_protocol.Connection.write connection message

let post_to_api ~sw ~env ~operation =
  let node = "http://localhost:8080/api/v1/operations" |> Uri.of_string in
  let json = Operation.yojson_of_t operation |> Yojson.Safe.to_string in
  let body = Piaf.Body.of_string json in
  let post_result = Piaf.Client.Oneshot.post ~body ~sw env node in
  match post_result with
  | Ok _ -> print_endline "operation submitted"
  | Error _ -> print_endline "FAIL to submit operation"

let make_identity secret =
  secret |> Secret.of_b58 |> Option.get |> Identity.make

type level_response = { level : Level.t } [@@deriving of_yojson]

let make_level ~sw ~env () =
  let response =
    Piaf.Client.Oneshot.get ~sw env
      (Uri.of_string "http://localhost:8080/api/v1/chain/level")
  in
  let body =
    match response with
    | Error _ -> failwith "cannot connect to the API"
    | Ok res -> res.body
  in
  let string = Piaf.Body.to_string body in
  let body =
    match string with
    | Error _ -> failwith "cannot parse body"
    | Ok body -> body
  in
  let yojson = Yojson.Safe.from_string body in
  let { level } = level_response_of_yojson yojson in
  level

let make_nonce () =
  let rng = Stdlib.Random.State.make_self_init () in
  Stdlib.Random.State.bits64 rng
  |> Int64.abs |> Z.of_int64 |> N.of_z |> Option.get |> Nonce.of_n

(* let print_op operation =
   operation |> Operation.yojson_of_t |> Yojson.Safe.to_string |> print_endline *)

let main ~env ~sw:_ =
  let identity =
    make_identity "edsk4UWkJqpZrAm26qvJE8uY9ZFGFqQiFuBcDyEPASXeHxuD68WvvF"
  in
  let level = Level.zero in
  let nonce = make_nonce () in

  (* Change this string with your appropriate needs*)
  let content =
    {|{
        "type": "mint",
        "operation": "cookie"}
    |}
  in

  let transaction =
    Operation.vm_transaction ~level ~nonce ~content ~identity
      ~chain_id:Deku_tezos.Address.empty
  in
  print_newline ();
  let _ = post_directly_to_node ~identity ~env ~operation:transaction in
  ()

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> main ~env ~sw
