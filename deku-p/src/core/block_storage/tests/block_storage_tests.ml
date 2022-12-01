open Deku_stdlib
open Deku_crypto
open Deku_consensus
open Deku_concepts

exception Test_finished

let block_testable = Alcotest.testable Block.pp Block.equal

let identity =
  let secret = Ed25519.Secret.generate () in
  let secret = Secret.Ed25519 secret in
  Identity.make secret

let block ~default_block_size =
  let above = Genesis.block in
  let withdrawal_handles_hash = BLAKE2b.hash "potato" in
  let producer = Producer.empty in
  Producer.produce ~identity ~default_block_size ~above ~withdrawal_handles_hash
    producer

let uri = Uri.of_string "sqlite3:/tmp/database.db"

let make_block_storage env sw =
  let domains = Eio.Stdenv.domain_mgr env in
  let worker = Parallel.Worker.make ~domains ~sw in
  let storage = Deku_block_storage.Block_storage.make ~worker ~uri in
  storage

let test_empty_block_load env () =
  try
    Eio.Switch.run @@ fun sw ->
    let block_storage = make_block_storage env sw in
    let (Block { hash; _ } as block) = block ~default_block_size:0 in
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
            ] );
        ])

let () = run ()
