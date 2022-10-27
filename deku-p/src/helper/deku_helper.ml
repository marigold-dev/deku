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
  let content =
    {| 
    {
      "type_": "Originate",
      "tickets": [],
      "content": {
        "module_": "\n(module\n  (import \"env\" \"dup_host\" (func $dup_host (param i64 ) (result)))\n(import \"env\" \"pair\" (func $pair (param i64 i64) (result i64)))\n(import \"env\" \"unpair\" (func $unpair (param i64)))\n(import \"env\" \"z_add\" (func $z_add (param i64 i64) (result i64)))\n(import \"env\" \"z_sub\" (func $z_sub (param i64 i64) (result i64)))\n(import \"env\" \"z_mul\" (func $z_mul (param i64 i64) (result i64)))\n(import \"env\" \"neg\" (func $neg (param i64) (result i64)))\n(import \"env\" \"lsl\" (func $lsl (param i64 i64) (result i64)))\n(import \"env\" \"concat\" (func $concat (param i64 i64) (result i64)))\n(import \"env\" \"lsr\" (func $lsr (param i64 i64) (result i64)))\n(import \"env\" \"compare\" (func $compare (param i64 i64) (result i64)))\n(import \"env\" \"car\" (func $car (param i64) (result i64)))\n(import \"env\" \"cdr\" (func $cdr (param i64) (result i64)))\n(import \"env\" \"some\" (func $some (param i64) (result i64)))\n(import \"env\" \"nil\" (func $nil (result i64)))\n(import \"env\" \"true\" (func $true (result i64)))\n(import \"env\" \"false\" (func $false (result i64)))\n(import \"env\" \"none\" (func $none (result i64)))\n(import \"env\" \"unit\" (func $unit (result i64)))\n(import \"env\" \"zero\" (func $zero (result i64)))\n(import \"env\" \"empty_map\" (func $empty_map (result i64)))\n(import \"env\" \"empty_set\" (func $empty_set (result i64)))\n(import \"env\" \"empty_big_map\" (func $empty_big_map (result i64)))\n(import \"env\" \"sender\" (func $sender (result i64)))\n(import \"env\" \"source\" (func $source (result i64)))\n(import \"env\" \"map_get\" (func $map_get (param i64 i64) (result i64)))\n(import \"env\" \"mem\" (func $mem (param i64 i64) (result i64)))\n(import \"env\" \"update\" (func $update (param i64 i64 i64) (result i64)))\n(import \"env\" \"iter\" (func $iter (param i64 i32) (result )))\n(import \"env\" \"map\" (func $map (param i64 i32) (result i64)))\n(import \"env\" \"if_left\" (func $if_left (param i64) (result i32)))\n(import \"env\" \"if_none\" (func $if_none (param i64) (result i32)))\n(import \"env\" \"if_cons\" (func $if_cons (param i64) (result i32)))\n(import \"env\" \"isnat\" (func $isnat (param i64) (result i64)))\n(import \"env\" \"not\" (func $not (param i64) (result i64)))\n(import \"env\" \"or\" (func $or (param i64 i64) (result i64)))\n(import \"env\" \"and\" (func $and (param i64 i64) (result i64)))\n(import \"env\" \"xor\" (func $xor (param i64 i64) (result i64)))\n(import \"env\" \"deref_bool\" (func $deref_bool (param i64) (result i32)))\n(import \"env\" \"neq\" (func $neq (param i64) (result i64)))\n(import \"env\" \"failwith\" (func $failwith (param i64)))\n(import \"env\" \"get_n\" (func $get_n (param i32 i64) (result i64)))\n(import \"env\" \"exec\" (func $exec (param i64 i64) (result i64)))\n(import \"env\" \"apply\" (func $apply (param i64 i64) (result i64)))\n(import \"env\" \"const\" (func $const (param i32) (result i64)))\n(import \"env\" \"abs\" (func $abs (param i64) (result i64)))\n(import \"env\" \"eq\" (func $eq (param i64) (result i64)))\n(import \"env\" \"gt\" (func $gt (param i64) (result i64)))\n(import \"env\" \"lt\" (func $lt (param i64) (result i64)))\n(import \"env\" \"closure\" (func $closure (param i32) (result i64)))\n(import \"env\" \"left\" (func $left (param i64) (result i64)))\n(import \"env\" \"right\" (func $right (param i64) (result i64)))\n(import \"env\" \"cons\" (func $cons (param i64 i64) (result i64)))\n(import \"env\" \"transfer_tokens\" (func $transfer_tokens (param i64 i64 i64) (result i64)))\n(import \"env\" \"address\" (func $address (param i64) (result i64)))\n(import \"env\" \"contract\" (func $contract (param i64) (result i64)))\n(import \"env\" \"self\" (func $self (result i64)))\n(import \"env\" \"self_address\" (func $self_address (result i64)))\n(import \"env\" \"get_and_update\" (func $get_and_update (param i64 i64 i64)))\n(import \"env\" \"read_ticket\" (func $read_ticket (param i64)))\n(import \"env\" \"ticket\" (func $ticket (param i64 i64) (result i64)))\n(import \"env\" \"join_tickets\" (func $join_tickets (param i64) (result i64)))\n(import \"env\" \"split_ticket\" (func $split_ticket (param i64 i64) (result i64)))\n(import \"env\" \"amount\" (func $amount (result i64)))\n(import \"env\" \"balance\" (func $balance (result i64)))\n(import \"env\" \"ediv\" (func $ediv (param i64 i64) (result i64)))\n(import \"env\" \"ge\" (func $ge (param i64) (result i64)))\n(import \"env\" \"le\" (func $le (param i64) (result i64)))\n(import \"env\" \"size\" (func $size (param i64) (result i64)))\n(import \"env\" \"int\" (func $int (param i64) (result i64)))\n(import \"env\" \"implicit_account\" (func $implicit_account (param i64) (result i64)))\n(import \"env\" \"blake2b\" (func $blake2b (param i64) (result i64)))\n(import \"env\" \"pack\" (func $pack (param i64) (result i64)))\n(import \"env\" \"unpack\" (func $unpack (param i64) (result i64)))\n(import \"env\" \"keccak\" (func $keccak (param i64) (result i64)))\n(import \"env\" \"sha256\" (func $sha256 (param i64) (result i64)))\n(import \"env\" \"sha3\" (func $sha3 (param i64) (result i64)))\n(import \"env\" \"sha512\" (func $sha512 (param i64) (result i64)))\n\n  (global $mode i32 (i32.const 0))\n\n  (memory 4)\n  (global $sp (mut i32) (i32.const 4000)) ;; stack pointer\n  (global $sh_sp (mut i32) (i32.const 1000)) ;;shadow_stack stack pointer\n\n  (global $__stack_base i32 (i32.const 32768))\n\n  (type $callback_t (func (param i64) (result i64)))\n  (func $call_callback (param $arg1 i64) (param $idx i32) (result i64)\n    (call_indirect (type $callback_t) (local.get $arg1) (local.get $idx)))\n\n  (type $callback_t_unit (func (param i64) (result)))\n  (func $call_callback_unit (param $arg1 i64) (param $idx i32) (result )\n    (call_indirect (type $callback_t_unit)\n      (local.get $arg1)\n      (local.get $idx)))\n\n  (func $dip (param $n i32) (result)\n    (local $stop i32)\n    (local $sp' i32)\n    (local $sh_sp' i32)\n    (local.set $stop (i32.const 0))\n    (local.set $sp'  (global.get $sp))\n    (local.tee $sh_sp' (i32.sub (global.get $sh_sp) (local.get $n)))\n    global.set $sh_sp\n    (loop $l\n      (i32.mul (i32.const 8) (i32.add (global.get $__stack_base) (i32.add (local.get $sh_sp') (local.get $stop))))\n      (i64.load (i32.mul (i32.const 8) (i32.add (local.get $sp') (local.get $stop))))\n      i64.store\n      (local.tee $stop (i32.add (local.get $stop) (i32.const 1)))\n      (local.get $n)\n      i32.ne\n      br_if $l)\n\n    (global.set $sp\n    (i32.add\n      (local.get $sp') (local.get $n))))\n\n  (func $undip (param $n i32) (result)\n    (local $stop i32)\n    (local $sp' i32)\n    (local $sh_sp' i32)\n    (local.tee $sp'  (i32.sub (global.get $sp) (local.get $n)))\n    global.set $sp\n    (local.set $sh_sp' (global.get $sh_sp))\n    (local.set $stop (i32.const 0))\n    (loop $l\n      (i32.mul (i32.const 8) (i32.add (local.get $sp') (local.get $stop)))\n      (i64.load\n        (i32.add\n          (global.get $__stack_base)\n          (i32.mul (i32.const 8) (i32.add (local.get $sh_sp') (local.get $stop)))))\n      (i64.store)\n      (local.tee $stop (i32.add (local.get $stop) (i32.const 1)))\n      (local.get $n)\n      i32.ne\n      br_if $l)\n    (global.set $sh_sp (i32.add (local.get $sh_sp') (local.get $n))))\n\n  (func $dup (param $n i32) (result)\n    (i64.load (i32.mul (i32.const 8) (i32.add (global.get $sp) (local.get $n))))\n    (call $dup_host))\n\n  (func $swap (param) (result)\n    (local $v1 i64)\n    (local $v2 i64)\n    (local.set $v1 (call $pop))\n    (local.set $v2 (call $pop))\n    (call $push (local.get $v1))\n    (call $push (local.get $v2)))\n\n  (func $dug (param $n i32) (result)\n    (local $idx i32)\n    (local $loop_idx i32)\n    (local $sp' i32)\n    (local $top i64)\n    (local.set $sp' (i32.add (global.get $sp) (local.get $n)))\n    (local.tee $idx (global.get $sp))\n    (local.tee $loop_idx)\n    (i32.mul (i32.const 8))\n    i64.load\n    local.set $top\n    (loop $loop\n      (i32.mul (i32.const 8) (local.get $idx))\n      (i32.add (local.get $loop_idx) (i32.const 1))\n      local.tee $loop_idx\n      (i32.mul (i32.const 8))\n      i64.load\n      i64.store\n      (local.set $idx (i32.add (local.get $idx) (i32.const 1)))\n      (local.get $idx)\n      (local.get $sp')\n      i32.lt_u\n      br_if $loop)\n\n    (i64.store (i32.mul (i32.const 8) (local.get $sp')) (local.get $top)))\n\n  (func $dig (param $n i32) (result)\n    (local $idx i32) (local $t i32) (local $digged i64)\n\n    (local.set $digged\n      (i64.load\n        (i32.mul (i32.const 8)\n          (local.tee $idx (i32.add (global.get $sp) (local.get $n))))))\n\n    (loop $loop\n      (local.set $t (i32.mul (i32.const 8) (local.get $idx)))\n\n      (i64.store (local.get $t)\n        (i64.load\n          (i32.mul\n            (i32.const 8)\n            (local.tee $idx (i32.sub (local.get $idx) (i32.const 1))))))\n\n      (br_if $loop\n        (i32.lt_u (global.get $sp) (local.get $idx))))\n\n    (i64.store (i32.mul (i32.const 8) (local.get $idx)) (local.get $digged)))\n\n  (func $pop (result i64)\n    (local $spp i32)\n    (i32.mul (i32.const 8) (local.tee $spp (global.get $sp)))\n    i64.load\n    (global.set $sp (i32.add (local.get $spp) (i32.const 1))))  ;;set stackptr\n\n  (func $push (param $value i64) (result)\n    (local $spp i32)\n    (i32.mul (i32.const 8) (local.tee $spp (i32.sub (global.get $sp) (i32.const 1)) ))\n    (i64.store (local.get $value))\n    (global.set $sp (local.get $spp)))  ;;set stackptr\n\n  (func $drop (param $n i32) (result)\n    (global.set $sp (i32.add (global.get $sp) (local.get $n))))  ;;set stackptr\n\n  (table $closures funcref (elem ))\n\n\n  (func $main (param $v1 i64) (result i64)\n    (local $1 i64)\n    (call $push (local.get $v1))\n    (call $unpair (call $pop)) ;; implicit return\n(call $if_left (call $pop)) (if (then (call $if_left (call $pop)) (if (then (call $swap)\n(call $push (call $z_sub (call $pop) (call $pop)))) (else (call $push (call $z_add (call $pop) (call $pop)))))) (else (call $drop (i32.const 2))\n(call $push (call $zero)) (; 0 ;)))\n(call $push (call $nil))\n(call $push (call $pair (call $pop) (call $pop)))\n    (call $pop))\n\n  (export \"push\" (func $push))\n  (export \"pop\" (func $push))\n  (export \"main\" (func $main))\n  (export \"closures\" (table $closures))\n  (export \"call_callback\" (func $call_callback))\n  (export \"call_callback_unit\" (func $call_callback_unit))\n  )\n",
        "constants": [],
        "initial_storage": [ "Int", "42" ],
        "entrypoints": {
          "%decrement": [ "Left", "Left" ],
          "%increment": [ "Left", "Right" ],
          "%reset": [ "Right" ]
        }
      }
    }
    |}
  in
  let operation = Yojson.Safe.from_string _content in
  let operation = Ocaml_wasm_vm.Operation_payload.t_of_yojson operation in
  print_endline (Ocaml_wasm_vm.Operation_payload.show operation);

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
