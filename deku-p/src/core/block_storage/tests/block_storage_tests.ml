open Deku_stdlib
open Deku_crypto
open Deku_consensus
open Deku_concepts
open Deku_gossip
open Deku_block_storage

exception Test_finished

let block_testable = Alcotest.testable Block.pp Block.equal

let vote_testable =
  Alcotest.testable Verified_signature.pp Verified_signature.equal

let identity =
  let secret = Ed25519.Secret.generate () in
  let secret = Secret.Ed25519 secret in
  Identity.make secret

let make_vote ~hash identity =
  let hash = Block_hash.to_blake2b hash in
  Verified_signature.sign hash identity

let block ~default_block_size =
  let above = Genesis.block in
  let withdrawal_handles_hash = BLAKE2b.hash "potato" in
  let producer = Producer.empty in
  Producer.produce ~identity ~default_block_size ~above ~withdrawal_handles_hash
    producer

(* NOTE: These tests generate new databases in /tmp/ for every test, for every run. *)
(* TODO: change to an in-memory databse *)
let uri () =
  let file_hash =
    let randn = Stdlib.Random.int 230 in
    Deku_crypto.BLAKE2b.hash (Int.to_string randn) |> BLAKE2b.to_hex
  in
  Uri.of_string (Format.sprintf "sqlite3:/tmp/%s.db" file_hash)

let make_block_storage env sw =
  let domains = Eio.Stdenv.domain_mgr env in
  let worker = Parallel.Worker.make ~domains ~sw in
  let storage = Block_storage.make ~worker ~uri:(uri ()) in
  storage

let test_empty_block_load env () =
  try
    Eio.Switch.run @@ fun sw ->
    let block_storage = make_block_storage env sw in
    let (Block { hash; level; _ } as block) = block ~default_block_size:0 in
    Deku_block_storage.Block_storage.save_block ~block block_storage;
    let retrieved_block =
      match
        Deku_block_storage.Block_storage.find_block_by_hash ~block_hash:hash
          block_storage
      with
      | Some block -> block
      | None -> Genesis.block
    in
    Alcotest.(check' block_testable)
      ~msg:"hash loaded block is not equal to saved block" ~expected:block
      ~actual:retrieved_block;
    let retrieved_block =
      match
        Deku_block_storage.Block_storage.find_block_by_level ~level
          block_storage
      with
      | Some block -> block
      | None -> Genesis.block
    in

    Alcotest.(check' block_testable)
      ~msg:"level loaded block is equal to saved block" ~expected:block
      ~actual:retrieved_block;

    Eio.Switch.fail sw Test_finished
  with _ -> ()

let test_empty_block_and_votes env () =
  try
    Eio.Switch.run @@ fun sw ->
    let block_storage = make_block_storage env sw in
    let (Block { hash; level; _ } as block) = block ~default_block_size:0 in
    let vote = make_vote ~hash identity in
    let votes = Verified_signature.Set.add vote Verified_signature.Set.empty in
    let votes = Verified_signature.Set.elements votes in
    let content = Deku_gossip.Message.Content.accepted ~block ~votes in
    let (Deku_gossip.Message.Message { header = _; content = _; network }) =
      Deku_gossip.Message.encode ~content
    in
    Block_storage.save_block_and_votes ~level ~network block_storage;

    let retrieved_block_and_votes =
      let default_return = (Genesis.block, []) in
      match
        Block_storage.find_block_and_votes_by_level ~level block_storage
      with
      | Some (Message.Network.Network_message { raw_header; raw_content }) -> (
          let expected = Message.Header.decode ~raw_header in
          let (Message.Message { content; _ }) =
            Message.decode ~expected ~raw_content
          in
          match content with
          | Content_accepted { block; votes } -> (block, votes)
          | _ -> default_return)
      | None -> default_return
    in
    Alcotest.(check' (pair block_testable (list vote_testable)))
      ~msg:"retrieved empty block and one vote equal saved"
      ~expected:(block, votes) ~actual:retrieved_block_and_votes;

    Eio.Switch.fail sw Test_finished
  with _ -> ()

let run () =
  Eio_main.run (fun env ->
      let open Alcotest in
      run "Block_storage" ~and_exit:false
        [
          ( "simple",
            [
              test_case "empty_block is returned" `Quick
                (test_empty_block_load env);
              test_case "empty block and one vote is returned" `Quick
                (test_empty_block_and_votes env);
            ] );
        ])

let () = run ()

(* TODO: Tests
   try all combinations of what's in the block_storage.mli. Use it with different block sizes, do it in parallel, try reading and writing at the same time, try reading a query that doesn't exist
   try reading something right before you write it and vice versa,
   try reading or writing two things at once *)
