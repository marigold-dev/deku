open Core_bench
open Protocol
open Wallet
open Crypto
open Tezos

let sk_1 = "spsk3LfH15rYByf7whY9YNAxS5ghpjzCr96jZ16Jt4pv2mnshf8Tcy"
let pk_1 = "sppk7aCim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U"

module TZ2_ex = struct
  let sk = Secret.of_string sk_1 |> Option.get
  let pk = Key.of_string pk_1 |> Option.get
end

let sk_2 = "p2sk2s8WFJdvL6J9JxY7R3tWwMnaqJotbcTFgXg3pSVNPvrxKL1AkQ"
let pk_2 = "p2pk67mCLEZ2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7"

module TZ3_ex = struct
  let sk = Secret.of_string sk_2 |> Option.get
  let pk = Key.of_string pk_2 |> Option.get
end

(*************************************************************************)
(* public key *)

let pk_genesis = "edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT6"
let edpk = genesis_wallet

(* NOTE TODO: Maybe I need to put the test of equality here?,
   note about the test does not check as in test when given a wrong value,
   it still pass. *)

let bench_keys_to_string =
  Bench.Test.create ~name:"key: to_string" (fun () ->
      let _ = to_string edpk in
      let _ = to_string TZ2_ex.pk in
      let _ = to_string TZ3_ex.pk in
      ())

let bench_keys_of_string =
  Bench.Test.create ~name:"key: of_string" (fun () ->
      let _ = of_string pk_genesis in
      let _ = of_string pk_1 in
      let _ = of_string pk_2 in
      ())

let bench_key_hashes =
  Bench.Test.create ~name:"key: hash" (fun () ->
      let open Key_hash in
      let _ = of_key genesis_wallet in
      let _ = of_key TZ2_ex.pk in
      let _ = of_key TZ3_ex.pk in
      ())

(*************************************************************************)
(* secret key *)

let sk_genesis = "edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd"
let edsk = genesis_key

let bench_secret_to_string =
  Bench.Test.create ~name:"secret: to_string" (fun () ->
      let open Secret in
      let _ = to_string edsk in
      let _ = to_string TZ2_ex.sk in
      let _ = to_string TZ3_ex.sk in
      ())

let bench_secret_of_string =
  Bench.Test.create ~name:"secret: of_string" (fun () ->
      let open Secret in
      let _ = of_string sk_genesis in
      let _ = of_string sk_1 in
      let _ = of_string sk_2 in
      ())

(*************************************************************************)
(* verify signature *)

let bench_verify_signature =
  Bench.Test.create ~name:"verify signature" (fun () ->
      let open BLAKE2B in
      let open Signature in
      let tuturu = hash "tuturu" in
      let edsig = sign edsk tuturu in
      let spsig = sign TZ2_ex.sk tuturu in
      let p2sign = sign TZ3_ex.sk tuturu in
      let _ = verify edpk edsig tuturu in
      let _ = verify TZ2_ex.pk spsig tuturu in
      let _ = verify TZ3_ex.pk p2sign tuturu in
      ())

(*************************************************************************)
(* contract hash *)

let contract_hash_string = "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc"

let contract_hash = Contract_hash.of_string contract_hash_string |> Option.get

let bench_contract_hash_to_string =
  Bench.Test.create ~name:"contract hash: to_string" (fun () ->
      let open Contract_hash in
      let kt1 = contract_hash in
      let _ = to_string kt1 in
      ())

let bench_contract_hash_of_string =
  Bench.Test.create ~name:"contract hash: of_string" (fun () ->
      let open Contract_hash in
      let _ = of_string contract_hash_string in
      ())

(*************************************************************************)
(* address *)

open Address

let tz1_string = "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC"
let tz1 = Implicit (Key_hash.of_key genesis_wallet)

let tz2_string = "tz2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD"
let tz2 = Implicit (Key_hash.of_key TZ2_ex.pk)

let tz3_string = "tz3b8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y"
let tz3 = Implicit (Key_hash.of_key TZ3_ex.pk)

let kt1 = Originated { contract = contract_hash; entrypoint = None }

let kt1_tuturu_string = "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc%tuturu"
let kt1_tuturu =
  Originated { contract = contract_hash; entrypoint = Some "tuturu" }

let bench_address_to_string =
  Bench.Test.create ~name:"address: to_string" (fun () ->
      let open Address in
      let _ = to_string tz1 in
      let _ = to_string tz2 in
      let _ = to_string tz3 in
      let _ = to_string kt1 in
      let _ = to_string kt1_tuturu in
      ())

let bench_address_of_string =
  Bench.Test.create ~name:"address: of_string" (fun () ->
      let open Address in
      let _ = of_string tz1_string in
      let _ = of_string tz2_string in
      let _ = of_string tz3_string in
      let _ = of_string contract_hash_string in
      let _ = of_string kt1_tuturu_string in
      ())

(*************************************************************************)
(* ticket *)

let bench_ticket_to_string =
  Bench.Test.create ~name:"ticket: to_string" (fun () ->
      let open Ticket_id in
      let kt1 =
        Address.Originated { contract = contract_hash; entrypoint = None } in
      let ticket = { ticketer = kt1; data = Bytes.of_string "a" } in
      let _ = to_string ticket in
      ())

let ticket_string = {|(Pair "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc" 0x61)|}

let bench_ticket_of_string =
  Bench.Test.create ~name:"ticket: of_string" (fun () ->
      let open Ticket_id in
      let _ = of_string ticket_string in
      ())

(*************************************************************************)
(* operation_hash *)

let op_hash_string = "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4"

let bench_operation_hash_string =
  Bench.Test.create ~name:"operation hash: to_string, of_string" (fun () ->
      let open Operation_hash in
      let op = of_string op_hash_string |> Option.get in
      let _ = to_string op in
      ())

(*************************************************************************)
(* operation forging *)

open Tezos
open Operation

let forge_transaction ~branch ~fee ~gas_limit ~storage_limit ~amount
    ~destination ~source ~counter ~secret ~entrypoint ~value =
  let branch = Block_hash.of_string branch |> Option.get in
  let fee = Tez.of_mutez fee |> Option.get in
  let gas_limit = Gas.of_int gas_limit |> Option.get in
  let storage_limit = Z.of_int storage_limit in
  let amount = Tez.of_mutez amount |> Option.get in
  let destination = Address.of_string destination |> Option.get in
  let source = Key_hash.of_string source |> Option.get in
  let counter = Z.of_int counter in
  let secret = Secret.of_string secret |> Option.get in
  let operation =
    {
      source;
      fee;
      counter;
      content = Transaction { amount; destination; entrypoint; value };
      gas_limit;
      storage_limit;
    } in
  Tezos.Operation.forge ~secret ~branch ~operations:[operation]

let branch = "BLBQQXyZ1qTwxZiT5FkJwvKj4YnCmx6xhqnhgaZg1Z13aQDJiCk"
let destination = "tz1ULf5uGJXefx8c8iLfHfuW1doMPpVicg7u"
let source = "tz1M6iKVFN8RhHjVSL3oF75nF2FJ1yMkrk5t"
let secret = "edsk4RbgwutwsEdVNuJsE5JDsxeJ6qFcG8F5rKFGnj5finT6FV46sd"

let bench_forge_transaction_taquito =
  Bench.Test.create ~name:"forge transaction taquito" (fun () ->
      let _ =
        forge_transaction ~branch ~fee:443L ~gas_limit:1520 ~storage_limit:0
          ~amount:2000_000L ~destination ~source ~counter:3_305_389 ~secret
          ~entrypoint:"default" ~value:Michelson.unit in
      ())

let branch = "BMb1r7vPdSkTb8ACDpuk4vKXPqEm6knqKjEzqpNj8Prxb2KWMP3"
let destination = "KT1GAr6WWLeavRVHgxEJq1F7tNLzavCLu9YB"

let bench_forge_transaction_bytes =
  Bench.Test.create ~name:"forge transaction bytes" (fun () ->
      let _ =
        forge_transaction ~branch ~fee:420L ~gas_limit:1303 ~storage_limit:0
          ~amount:0L ~destination ~source ~secret ~counter:3_305_396
          ~entrypoint:"default" ~value:Michelson.unit in
      ())

(*************************************************************************)
(* pack *)

open Pack

let bytes s () = bytes (Bytes.of_string (Hex.to_string (`Hex s)))

let bench_pack_int_1 =
  Bench.Test.create ~name:"pack: int 1" (fun () ->
      let _ = int (Z.of_int 1) in
      ())

let bench_pack_int_minus_1 =
  Bench.Test.create ~name:"pack: int -1" (fun () ->
      let _ = int (Z.of_int (-1)) in
      ())

let bench_pack_bytes_0x =
  Bench.Test.create ~name:"pack: bytes 0x" (fun () ->
      let _ = bytes "" () in
      ())

let bench_pack_bytes_050001 =
  Bench.Test.create ~name:"pack: bytes 050001" (fun () ->
      let _ = bytes "050001" () in
      ())

let bench_pack_pair_int_bytes =
  Bench.Test.create ~name:"pack: pair: (1, 0x)" (fun () ->
      let _ = pair (int (Z.of_int 1)) (bytes "" ()) in
      ())

let bench_pack_pair_pair =
  Bench.Test.create ~name:"pack: (1, (0xAA, -1))" (fun () ->
      let _ =
        pair (int (Z.of_int 1)) (pair (bytes "AA" ()) (int (Z.of_int (-1))))
      in
      ())

let bench_pack_list_empty =
  Bench.Test.create ~name:"pack: list empty" (fun () ->
      let _ = list [] in
      ())

let bench_pack_list_int =
  Bench.Test.create ~name:"pack: list int 1" (fun () ->
      let _ = list [int (Z.of_int 1)] in
      ())

let bench_pack_list_pair =
  Bench.Test.create ~name:"pack: list pair" (fun () ->
      let _ =
        list
          [pair (int (Z.of_int 1)) (pair (bytes "" ()) (int (Z.of_int (-1))))]
      in
      ())

let bench_pack_key =
  Bench.Test.create ~name:"pack: key" (fun () ->
      let _ = key genesis_wallet in
      ())

let bench_pack_key_hash =
  Bench.Test.create ~name:"pack: key_hash" (fun () ->
      let _ = key_hash (Key_hash.of_key genesis_wallet) in
      ())

let bench_pack_address_implicit =
  Bench.Test.create ~name:"pack: address implicit" (fun () ->
      let _ = address (Implicit (Key_hash.of_key genesis_wallet)) in
      ())

let bench_pack_address_originated =
  Bench.Test.create ~name:"pack: address originated" (fun () ->
      let _ =
        address (Originated { contract = contract_hash; entrypoint = None })
      in
      ())

let bench_pack_to_bytes_int =
  Bench.Test.create ~name:"pack: to_bytes int" (fun () ->
      let pack = int (Z.of_int 1) in
      let _ = to_bytes pack in
      ())

let bench_pack_to_bytes_bytes =
  Bench.Test.create ~name:"pack: to_bytes bytes" (fun () ->
      let pack = bytes "" () in
      let _ = to_bytes pack in
      ())

let bench_pack_to_bytes_pair =
  Bench.Test.create ~name:"pack: to_bytes pair" (fun () ->
      let pack = pair (int (Z.of_int 1)) (bytes "" ()) in
      let _ = to_bytes pack in
      ())

let bench_pack_to_bytes_list =
  Bench.Test.create ~name:"pack: to_bytes list" (fun () ->
      let pack = list [] in
      let _ = to_bytes pack in
      ())

let bench_pack_to_bytes_key =
  Bench.Test.create ~name:"pack: to_bytes key" (fun () ->
      let pack = key genesis_wallet in
      let _ = to_bytes pack in
      ())

let bench_pack_to_bytes_key_hash =
  Bench.Test.create ~name:"pack: to_bytes key_hash" (fun () ->
      let pack = key_hash (Key_hash.of_key genesis_wallet) in
      let _ = to_bytes pack in
      ())

let bench_pack_to_bytes_address_implicit =
  Bench.Test.create ~name:"pack: to_bytes address implicit" (fun () ->
      let pack = address (Implicit (Key_hash.of_key genesis_wallet)) in
      let _ = to_bytes pack in
      ())

let bench_pack_to_bytes_address_originated =
  Bench.Test.create ~name:"pack: to_bytes address originated" (fun () ->
      let pack =
        address (Originated { contract = contract_hash; entrypoint = None })
      in
      let _ = to_bytes pack in
      ())

(*************************************************************************)
(* consensus *)
open Helpers
open Deku.Consensus

let hash_exn s = BLAKE2B.of_string s |> Option.get

let key_hash_exn s = Key_hash.of_string s |> Option.get

let address_exn s = Address.of_string s |> Option.get

let hash_exn_1 =
  "2d92960a592c56de3046e200969c230a2eda71fc4b775e0cc09a189e5ddc5dbd"
let hash_exn_2 =
  "bdd051ddb07925a0d88dc27583e38ae560aa1b4429cc93b9ec35dacdbd74ffb2"
let hash_exn_3 =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
let hash_exn_4 =
  "546d2bb2375cc919efc81a103a7ad3bd1227546b320f275e357bd9a5d5eef946"

let key_hash_1 = "tz1XoDYhrUJT4HtskbEUrJusHtFHx6ZXcemd"
let key_hash_2 = "tz1R1XF4NnYkiCxcVphdLTYokQiyL38rtSQF"
let key_hash_3 = "tz1d6QHk2oFzrYYasZWof8BU26D7jXAXeajv"
let key_hash_4 = "tz1da6gqyddChGTwzW5aUA3Bia7DaAXmtqAE"

let address_exn_1 = "tz1YywYq77UAMbVgoYndnZLkRawjUhX3nVh4"
let address_exn_2 = "KT1AS9rCk1wpybsvZ5Tnd4yRxDvtN39uxMoq"

let bench_key_hash_exn =
  Bench.Test.create ~name:"consensus: key_hash_exn" (fun () ->
      let _ = key_hash_exn key_hash_1 in
      ())

let bench_address_exn =
  Bench.Test.create ~name:"consensus: address_exn" (fun () ->
      let _ = address_exn address_exn_1 in
      ())

let bench_hash_validators =
  Bench.Test.create ~name:"consensus: hash_validators" (fun () ->
      let hash =
        [
          key_hash_exn key_hash_1;
          key_hash_exn key_hash_2;
          key_hash_exn key_hash_3;
          key_hash_exn key_hash_4;
        ]
        |> hash_validators in
      let _hash = BLAKE2B.to_string hash in
      ())

let bench_hash_block =
  Bench.Test.create ~name:"consensus: hash_block" (fun () ->
      let hash =
        hash_block ~block_height:121L ~block_payload_hash:(hash_exn hash_exn_1)
          ~state_root_hash:(hash_exn hash_exn_2)
          ~withdrawal_handles_hash:(hash_exn hash_exn_3)
          ~validators_hash:(hash_exn hash_exn_4) in
      let _hash = BLAKE2B.to_string hash in
      ())

let bench_hash_withdraw_handle =
  Bench.Test.create ~name:"consensus: hash_withdraw_handle" (fun () ->
      let hash =
        hash_withdraw_handle ~id:(Z.of_int 0)
          ~owner:(address_exn address_exn_1)
          ~amount:(Z.of_int 10)
          ~ticketer:(address_exn address_exn_2)
          ~data:(Bytes.of_string "") in
      let _ = BLAKE2B.to_string hash in
      ())

(*************************************************************************)
(* discovery *)

let bench_discovery =
  Bench.Test.create ~name:"discovery" (fun () ->
      let open Tezos_interop in
      let open Discovery in
      let _signature =
        sign genesis_key ~nonce:1L (Uri.of_string "http://localhost") in
      ())

(*************************************************************************)

let tests =
  [
    bench_keys_to_string;
    bench_keys_of_string;
    bench_key_hashes;
    bench_secret_to_string;
    bench_secret_of_string;
    bench_verify_signature;
    bench_contract_hash_to_string;
    bench_contract_hash_of_string;
    bench_address_to_string;
    bench_address_of_string;
    bench_ticket_to_string;
    bench_ticket_of_string;
    bench_address_of_string;
    bench_operation_hash_string;
    bench_forge_transaction_taquito;
    bench_forge_transaction_bytes;
    bench_pack_int_1;
    bench_pack_int_minus_1;
    bench_pack_bytes_0x;
    bench_pack_bytes_050001;
    bench_pack_pair_int_bytes;
    bench_pack_pair_pair;
    bench_pack_list_empty;
    bench_pack_list_int;
    bench_pack_list_pair;
    bench_pack_key;
    bench_pack_key_hash;
    bench_pack_address_implicit;
    bench_pack_address_originated;
    bench_pack_to_bytes_int;
    bench_pack_to_bytes_bytes;
    bench_pack_to_bytes_list;
    bench_pack_to_bytes_pair;
    bench_pack_to_bytes_key;
    bench_pack_to_bytes_key_hash;
    bench_pack_to_bytes_address_implicit;
    bench_pack_to_bytes_address_originated;
    bench_address_exn;
    bench_key_hash_exn;
    bench_hash_validators;
    bench_hash_block;
    bench_hash_withdraw_handle;
    bench_discovery;
  ]

let command = Bench.make_command tests