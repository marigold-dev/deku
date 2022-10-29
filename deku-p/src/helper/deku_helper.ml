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
  let json = Operation.Signed.yojson_of_t operation |> Yojson.Safe.to_string in
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
  let _content2 =
    {|   {"operation":"{ \"initial_storage\": [ \"Int\", \"5\" ],\n  \"module\":\n    \"0061736d0100000001c3808080000d60017e017e60017e0060027e7e017e6000017e60037e7e7e017e60027e7f0060027e7f017e60017e017f60027f7e017e60017f017e60037e7e7e0060017f0060000002e0878080004e03656e76086475705f686f7374000103656e760470616972000203656e7606756e70616972000103656e76057a5f616464000203656e76057a5f737562000203656e76057a5f6d756c000203656e76036e6567000003656e76036c736c000203656e7606636f6e636174000203656e76036c7372000203656e7607636f6d70617265000203656e7603636172000003656e7603636472000003656e7604736f6d65000003656e76036e696c000303656e760474727565000303656e760566616c7365000303656e76046e6f6e65000303656e7604756e6974000303656e76047a65726f000303656e7609656d7074795f6d6170000303656e7609656d7074795f736574000303656e760d656d7074795f6269675f6d6170000303656e760673656e646572000303656e7606736f75726365000303656e76076d61705f676574000203656e76036d656d000203656e7606757064617465000403656e760469746572000503656e76036d6170000603656e760769665f6c656674000703656e760769665f6e6f6e65000703656e760769665f636f6e73000703656e760569736e6174000003656e76036e6f74000003656e76026f72000203656e7603616e64000203656e7603786f72000203656e760a64657265665f626f6f6c000703656e76036e6571000003656e76086661696c77697468000103656e76056765745f6e000803656e760465786563000203656e76056170706c79000203656e7605636f6e7374000903656e7603616273000003656e76026571000003656e76026774000003656e76026c74000003656e7607636c6f73757265000903656e76046c656674000003656e76057269676874000003656e7604636f6e73000203656e760f7472616e736665725f746f6b656e73000403656e760761646472657373000003656e7608636f6e7472616374000003656e760473656c66000303656e760c73656c665f61646472657373000303656e760e6765745f616e645f757064617465000a03656e760b726561645f7469636b6574000103656e76067469636b6574000203656e760c6a6f696e5f7469636b657473000003656e760c73706c69745f7469636b6574000203656e7606616d6f756e74000303656e760762616c616e6365000303656e760465646976000203656e76026765000003656e76026c65000003656e760473697a65000003656e7603696e74000003656e7610696d706c696369745f6163636f756e74000003656e7607626c616b653262000003656e76047061636b000003656e7606756e7061636b000003656e76066b656363616b000003656e7606736861323536000003656e760473686133000003656e76067368613531320000038d808080000c06050b0b0b0c0b0b03010b000485808080000170010000058380808000010004069980808000047f0041000b7f0141a01f0b7f0141e8070b7f00418080020b07c580808000060470757368005703706f700057046d61696e005908636c6f737572657301000d63616c6c5f63616c6c6261636b004e1263616c6c5f63616c6c6261636b5f756e6974004f098680808000010041000b000a80848080000c898080800000200020011100000b898080800000200020011101000bc48080800001037f4100210123012102230220006b22032402034041082303200320016a6a6c4108200220016a6c290300370300200141016a22012000470d000b200220006a24010bc48080800001037f230120006b22022401230221034100210103404108200220016a6c23034108200320016a6c6a290300370300200141016a22012000470d000b200320006a24020b8f80808000004108230120006a6c29030010000b948080800001027e105621001056210120001057200110570bcb8080800002037f017e230120006a210323012201220241086c29030021040340410820016c200241016a220241086c290300370300200141016a210120012003490d000b410820036c20043703000bc28080800002027f017e4108230120006a22016c29030021030340410820016c210220024108200141016b22016c29030037030023012001490d000b410820016c20033703000b958080800001017f4108230122006c290300200041016a24010b978080800001017f4108230141016b22016c2000370300200124010b898080800000230120006a24010bc48080800001017e20001057105610021056101e04401056101e0440105310561056100410570510561056100310570b0541021058101310570b100e1057105610561001105710560b\",\n  \"constants\": [],\n  \"entrypoints\":\n    { \"%decrement\": [ \"Left\", \"Left\" ], \"%increment\": [ \"Left\", \"Right\" ],\n      \"%reset\": [ \"Right\" ] } }","tickets":[]}
|}
  in
  (* Change this string with your appropriate needs*)
  let _content =
    {| 
      {"operation":"{ \"address\": \"DK1AtSE7kmY88z1trtSb2FPCjCmiRkqtKXhm\",\n  \"argument\":\n    [ \"Union\", [ \"Left\", [ \"Union\", [ \"Right\", [ \"Int\", \"5\" ] ] ] ] ] }","tickets":[]}
    |}
  in
  let operation = Yojson.Safe.from_string _content in
  let operation = Ocaml_wasm_vm.Operation.t_of_yojson operation in
  print_endline (Ocaml_wasm_vm.Operation.show operation);

  let (Deku_protocol.Operation.Signed.Signed_operation transaction as op) =
    Operation.Signed.vm_transaction ~level ~nonce ~content:operation ~identity
  in
  let (Deku_protocol.Operation.Initial.Initial_operation { hash; _ }) =
    transaction.initial
  in
  let address =
    Deku_ledger.Contract_address.of_user_operation_hash
      (Deku_protocol.Operation_hash.to_blake2b hash)
    |> Deku_ledger.Contract_address.to_b58
  in
  print_newline ();
  print_endline ("Address: " ^ address ^ "\n");
  print_newline ();
  let _ = post_directly_to_node ~identity ~env ~operation:op in
  ()

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> main ~env ~sw
