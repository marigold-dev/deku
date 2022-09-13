open Deku_consensus
open Deku_crypto
open Deku_tezos
open Deku_concepts

(* FIXME: I broke these ligo-generated hashes - need to update these comments. *)

(* TODO: for now this only test a predefined withdrawal_handles_hash, namely
   Deku_crypto.BLAKE2b.hash "tuturu" |> Deku_crypto.BLAKE2b.to_hex;;
   - : string = "b55ce6d1804e12b112c9795f18b81d2ec7ff33047e67a05e0c8603c5e49c3203" *)

(* Derivied with the following code using `ligo repel cameligo`
    #use "./src/tezos_interop/consensus.mligo";;
    Crypto.blake2b (Bytes.pack ({ block_level = (1 : int);
      block_payload_hash = (0x99586ffad9cfcfe056c7374a42820faec628e7ee5d1bd8cc5ec23ce67f51c5ba : blake2b);
      state_hash = (0xfa301e3ff218de7844b21dd7364a444243f1772f6eba158a4a3a911b59da7d8c : blake2b);
      handles_hash = (0xb55ce6d1804e12b112c9795f18b81d2ec7ff33047e67a05e0c8603c5e49c3203: blake2b) } : block_hash_structure));;

   This expression outputs 0x36b832fb5aff5cba4ad4075cf7f6d6d20a83c793f8e550cb18a559f1a9c8355e

    FIXME: This could be automated according to https://tezos-dev.slack.com/archives/CFX0B8Q3X/p1659802632591539?thread_ts=1659800515.479659&cid=CFX0B8Q3X
*)

(* This  *)
let expected_block_structure_hash =
  "36b832fb5aff5cba4ad4075cf7f6d6d20a83c793f8e550cb18a559f1a9c8355e"

let expected_block_hash =
  "6d6804725b0d680eb81b50439d75e3fa67ea45f0d644fc91eb0f9ad5fa0e110e"

let test_deku_block_hashing () =
  let level = Level.zero |> Level.next in
  let block_payload_hash =
    BLAKE2b.of_hex
      "99586ffad9cfcfe056c7374a42820faec628e7ee5d1bd8cc5ec23ce67f51c5ba"
    |> Option.get
  in
  let state_root_hash =
    BLAKE2b.of_hex
      "fa301e3ff218de7844b21dd7364a444243f1772f6eba158a4a3a911b59da7d8c"
    |> Option.get
  in
  let withdrawal_handles_hash =
    BLAKE2b.of_hex
      "b55ce6d1804e12b112c9795f18b81d2ec7ff33047e67a05e0c8603c5e49c3203"
    |> Option.get
  in
  let hash =
    Deku.Consensus.hash_block ~block_level:level ~block_payload_hash
      ~state_root_hash ~withdrawal_handles_hash
    |> BLAKE2b.to_hex
  in
  Alcotest.(
    check string "Deku block hashing matches Tezos block hashing"
      expected_block_structure_hash hash)

open Alcotest

let test_consensus_block_hashing () =
  let (Block.Block { hash; _ }) = Fixme_name.some_block in
  let hash = Block_hash.to_blake2b hash |> BLAKE2b.to_hex in
  let expected_hash =
    BLAKE2b.of_hex expected_block_structure_hash
    |> Option.get |> BLAKE2b.to_raw |> BLAKE2b.hash |> BLAKE2b.to_hex
  in
  Alcotest.(
    check string "Deku consensus block hashing matches Tezos block hashing"
      expected_hash hash)

let test_basic_signing () =
  (* I checked that this works with ligo repl:

     Taken from the key: edsk4UWkJqpZrAm26qvJE8uY9ZFGFqQiFuBcDyEPASXeHxuD68WvvF (first validator)

     Crypto.check
       ("edpku9AgudAnEYeuf2UUydQ55VLffFp2bFQ1TRtMC4oMAs61wMUJM3" : key)
       ("edsigtwmU8KFJf61hs2N5oSnFKzEDo83pHZJrTALN6Xy8dV9py5punbX3fmN4RezZhn1V2rUbh3Ej5oiCSxakiLhQsDeA6pJ7oX" : signature)
       (Bytes.pack 3);;

     So now we want to verify that doing the same in Deku produces an identical signature
  *)
  let data = Z.of_int 3 |> Pack.int in
  let packed_data_hash = Deku.Consensus.hash_packed_data data in
  let identity = List.hd Fixme_name.identities in
  let signature =
    Verified_signature.sign packed_data_hash identity
    |> Verified_signature.signature |> Signature.to_b58
  in
  let expected_signature =
    "edsigtwmU8KFJf61hs2N5oSnFKzEDo83pHZJrTALN6Xy8dV9py5punbX3fmN4RezZhn1V2rUbh3Ej5oiCSxakiLhQsDeA6pJ7oX"
  in
  Alcotest.(
    check string "Deku signing is Tezos compatible" expected_signature signature)

let test_signing_a_hash () =
  (* I checked that this works with ligo repl:

     Taken from the key: edsk4UWkJqpZrAm26qvJE8uY9ZFGFqQiFuBcDyEPASXeHxuD68WvvF (first validator)

     Crypto.check
       ("edpku9AgudAnEYeuf2UUydQ55VLffFp2bFQ1TRtMC4oMAs61wMUJM3" : key)
       ("edsigts1rT7eR6HnhN5Hwdy8HeUrg1pQrxw2w2a3Ru9NHL6G4pk4RABUXZfdBFccMQBpdWKy2F53Bc21SyszhHB1zwhc73Uskud" : signature)
       (Crypto.blake2b (Bytes.pack 3));;

     So now we want to verify that doing the same in Deku produces an identical signature
  *)
  let data = Z.of_int 3 |> Pack.int in
  let data_hash = Deku.Consensus.hash_packed_data data in
  let hash_to_verify = BLAKE2b.to_raw data_hash |> BLAKE2b.hash in
  let identity = List.hd Fixme_name.identities in
  let signature =
    Verified_signature.sign hash_to_verify identity
    |> Verified_signature.signature |> Signature.to_b58
  in
  let expected_signature =
    "edsigts1rT7eR6HnhN5Hwdy8HeUrg1pQrxw2w2a3Ru9NHL6G4pk4RABUXZfdBFccMQBpdWKy2F53Bc21SyszhHB1zwhc73Uskud"
  in
  Alcotest.(
    check string "Deku signing is Tezos compatible" expected_signature signature)

let test_block_signing () =
  (* We know from the other tests that this hash is the same one
     that Tezos derives for the block.

     Hence, we check the following on Ligo repl:

     Crypto.check
       ("edpku9AgudAnEYeuf2UUydQ55VLffFp2bFQ1TRtMC4oMAs61wMUJM3" : key)
       ("edsigtwfg8hjDdAEKY8j9Yv7zQhnBaFKUtpvhYrKk9ipP4hzMDcWndgMdqQBBtKkDFBftfiGaQ6VZaNxH3eRqLJ7WLUqSETVi13": signature)
       (0x36b832fb5aff5cba4ad4075cf7f6d6d20a83c793f8e550cb18a559f1a9c8355e : bytes);;
  *)
  let hash = BLAKE2b.of_hex expected_block_structure_hash |> Option.get in
  let hash_to_verify = BLAKE2b.to_raw hash |> BLAKE2b.hash in
  Alcotest.(
    check string "Sanity check of hash to verify" expected_block_hash
      (BLAKE2b.to_hex hash_to_verify));
  let identity = List.hd Fixme_name.identities in
  let signature =
    Verified_signature.sign hash_to_verify identity
    |> Verified_signature.signature |> Signature.to_b58
  in
  let expected_signature =
    "edsigtwfg8hjDdAEKY8j9Yv7zQhnBaFKUtpvhYrKk9ipP4hzMDcWndgMdqQBBtKkDFBftfiGaQ6VZaNxH3eRqLJ7WLUqSETVi13"
  in
  Alcotest.(
    check string "Deku block signing is Tezos compatible" expected_signature
      signature);
  (* Now that we've tested constructing the hashes by hand, we test that
     Block.sign produces an identical signature. *)
  let signature =
    Block.sign ~identity Fixme_name.some_block
    |> Verified_signature.signature |> Signature.to_b58
  in
  Alcotest.(
    check string "Deku block signing is correct" expected_signature signature)

let () =
  run "Deku_tezos_interop" ~and_exit:false
    [
      ( "hashing",
        [
          test_case "Deku block hashing" `Quick test_deku_block_hashing;
          test_case "Deku consensus block hashing" `Quick
            test_consensus_block_hashing;
        ] );
      ( "signing",
        [
          test_case "Deku basic signing" `Quick test_basic_signing;
          test_case "Deku signing a hash" `Quick test_signing_a_hash;
          test_case "Deku block signing" `Quick test_block_signing;
        ] );
    ]
