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

let contract_hash_string = "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc"

let contract_hash = Contract_hash.of_string contract_hash_string |> Option.get

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

let op_hash_string = "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4"

let bench_operation_hash_string =
  Bench.Test.create ~name:"operation hash: to_string, of_string" (fun () ->
      let open Operation_hash in
      let op = of_string op_hash_string |> Option.get in
      let _ = to_string op in
      ())

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
    bench_address_of_string;
    bench_operation_hash_string;
  ]

let command = Bench.make_command tests