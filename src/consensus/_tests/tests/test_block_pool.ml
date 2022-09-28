open Deku_crypto
open Deku_concepts
open Deku_consensus
open Block_pool

(* TODO: do generative testing,
   properties:
    append_vote -> append_block = append_block -> append_vote *)
let identity () =
  let secret = Ed25519.Secret.generate () in
  let secret = Secret.Ed25519 secret in
  Identity.make secret

let make_block previous =
  let open Block in
  let identity = identity () in
  let (Block { hash = previous; level = previous_level; _ }) = previous in
  let level = Level.next previous_level in
  let operations = [] in
  let withdrawal_handles_hash = Deku_crypto.BLAKE2b.hash "tuturu" in
  Block.produce ~parallel_map:List.map ~identity ~level ~previous ~operations
    ~tezos_operations:[] ~withdrawal_handles_hash

let make_vote () =
  let identity = identity () in
  let hash = BLAKE2b.hash "kyyyyyouma" in
  Verified_signature.sign hash identity

let size pool =
  let (Pool { by_hash; by_previous }) = pool in
  Block_hash.Map.cardinal by_hash + Block_hash.Map.cardinal by_previous

let ensure msg actual = Alcotest.(check' bool) ~msg ~expected:true ~actual
let test_empty_is_empty () = ensure {|size empty = 0|} (size empty = 0)

let test_append_block_appends_to_both () =
  let (Block { hash; previous; _ } as block) = Genesis.block in
  let pool = append_block ~block empty in
  ensure {|size (append_block empty) = 2|} (size pool = 2);
  ensure {|find_block (append empty) = Some block|}
    (find_block ~hash pool = Some block);
  ensure {|find_next (append empty) = [block]|}
    (find_next ~hash:previous pool = Block.Set.of_list [ block ]);
  ensure {|find_votes (append empty) = []|}
    (find_votes ~hash pool = Key_hash.Map.empty)

let test_double_append_block_is_noop () =
  let block = Genesis.block in
  let append_block pool = append_block ~block pool in
  ensure {|append_block (append_block empty) = append_block empty|}
    (append_block (append_block empty) = append_block empty)

let test_append_block_with_dangling_votes () =
  let (Block { hash; previous; _ } as block) = Genesis.block in
  let pool = empty in

  let vote = make_vote () in
  let validator = Verified_signature.key_hash vote in
  let pool = append_vote ~vote ~hash pool in

  ensure {|size (append_vote empty) = 1|} (size pool = 1);
  ensure {|find_block (append_vote empty) = None|} (find_block ~hash pool = None);
  ensure {|find_next (append empty) = []|}
    (find_next ~hash:previous pool = Block.Set.empty);
  ensure {|find_votes (append empty) = [validator]|}
    (find_votes ~hash pool = Key_hash.Map.singleton validator vote);

  let pool = append_block ~block pool in
  ensure {|size (append_block (append_vote empty)) = 2|} (size pool = 2);
  ensure {|find_block (append_block (append_vote empty)) = Some block|}
    (find_block ~hash pool = Some block);
  ensure {|find_next (append empty) = [block]|}
    (find_next ~hash:previous pool = Block.Set.of_list [ block ]);
  ensure {|find_votes (append empty) = [validator]|}
    (find_votes ~hash pool = Key_hash.Map.singleton validator vote)

let test_appends_two_different_blocks () =
  let (Block { hash = hash_a; previous = previous_a; _ } as block_a) =
    make_block Genesis.block
  in
  let (Block { hash = hash_b; previous = previous_b; _ } as block_b) =
    make_block block_a
  in
  assert (previous_b = hash_a);

  let pool = empty in
  let pool = append_block ~block:block_a pool in

  ensure {|size (append_block_a empty) = 2|} (size pool = 2);
  ensure {|find_block_a (append_block_a empty) = Some block_a|}
    (find_block ~hash:hash_a pool = Some block_a);
  ensure {|find_block_b (append_block_a empty) = None|}
    (find_block ~hash:hash_b pool = None);
  ensure {|find_next_genesis (append_block_a empty) = [block_a]|}
    (find_next ~hash:previous_a pool = Block.Set.of_list [ block_a ]);
  ensure {|find_next_a (append_block_a empty) = []|}
    (find_next ~hash:hash_a pool = Block.Set.empty);
  ensure {|find_next_b (append_block_a empty) = []|}
    (find_next ~hash:hash_b pool = Block.Set.empty);
  ensure {|find_votes_a (append_block_a empty) = []|}
    (find_votes ~hash:hash_a pool = Key_hash.Map.empty);
  ensure {|find_votes_b (append_block_b empty) = []|}
    (find_votes ~hash:hash_b pool = Key_hash.Map.empty);

  let pool = append_block ~block:block_b pool in
  ensure {|size (append_block_b (append_block_a empty)) = 4|} (size pool = 4);
  ensure {|find_block_a (append_block_b (append_block_a empty)) = Some block_a|}
    (find_block ~hash:hash_a pool = Some block_a);
  ensure {|find_block_b (append_block_b (append_block_a empty)) = Some block_b|}
    (find_block ~hash:hash_b pool = Some block_b);
  ensure
    {|find_next_genesis (append_block_b (append_block_a empty)) = [block_a]|}
    (find_next ~hash:previous_a pool = Block.Set.of_list [ block_a ]);
  ensure {|find_next_a (append_block_b (append_block_a empty)) = [block_b]|}
    (find_next ~hash:hash_a pool = Block.Set.of_list [ block_b ]);
  ensure {|find_next_b (append_block_b (append_block_a empty)) = []|}
    (find_next ~hash:hash_b pool = Block.Set.empty);
  ensure {|find_votes_a (append_block_b (append_block_a empty)) = []|}
    (find_votes ~hash:hash_a pool = Key_hash.Map.empty);
  ensure {|find_votes_b (append_block_b empty) = []|}
    (find_votes ~hash:hash_b pool = Key_hash.Map.empty)

let test_remove_block () =
  let (Block { hash = hash_a; previous = previous_a; _ } as block_a) =
    make_block Genesis.block
  in
  let (Block { hash = hash_b; previous = previous_b; _ } as block_b) =
    make_block block_a
  in
  assert (previous_b = hash_a);

  let pool = empty in
  let pool = append_block ~block:block_a pool in
  let pool = append_block ~block:block_b pool in
  let vote = make_vote () in
  let validator = Verified_signature.key_hash vote in
  let pool = append_vote ~vote ~hash:hash_a pool in

  ensure {|size pool_blocks_and_vote = 4|} (size pool = 4);
  ensure {|find_block_a pool_blocks_and_vote = Some block_a|}
    (find_block ~hash:hash_a pool = Some block_a);
  ensure {|find_block_b pool_blocks_and_vote = Some block_b|}
    (find_block ~hash:hash_b pool = Some block_b);
  ensure {|find_next_genesis (append_block_a empty) = [block_a]|}
    (find_next ~hash:previous_a pool = Block.Set.of_list [ block_a ]);
  ensure {|find_next_a pool_blocks_and_vote = [block_b]|}
    (find_next ~hash:hash_a pool = Block.Set.of_list [ block_b ]);
  ensure {|find_next_b pool_blocks_and_vote = []|}
    (find_next ~hash:hash_b pool = Block.Set.empty);
  ensure {|find_votes_a pool_blocks_and_vote = [validator]|}
    (find_votes ~hash:hash_a pool = Key_hash.Map.singleton validator vote);
  ensure {|find_votes_b pool_blocks_and_vote = []|}
    (find_votes ~hash:hash_b pool = Key_hash.Map.empty);

  let pool = remove ~block:block_a pool in
  ensure {|size (remove_a pool_blocks_and_vote) = 2|} (size pool = 2);
  ensure {|find_block_a (remove_a pool_blocks_and_vote) = None|}
    (find_block ~hash:hash_a pool = None);
  ensure {|find_block_b (remove_a pool_blocks_and_vote) = Some block_b|}
    (find_block ~hash:hash_b pool = Some block_b);
  ensure {|find_next_genesis (remove_a (append_block_a empty)) = []|}
    (find_next ~hash:previous_a pool = Block.Set.of_list []);
  ensure {|find_next_a (remove_a pool_blocks_and_vote) = [block_b]|}
    (find_next ~hash:hash_a pool = Block.Set.of_list [ block_b ]);
  ensure {|find_next_b (remove_a pool_blocks_and_vote) = []|}
    (find_next ~hash:hash_b pool = Block.Set.empty);
  ensure {|find_votes_a (remove_a pool_blocks_and_vote) = []|}
    (find_votes ~hash:hash_a pool = Key_hash.Map.empty);
  ensure {|find_votes_b (remove_a pool_blocks_and_vote) = []|}
    (find_votes ~hash:hash_b pool = Key_hash.Map.empty)

let run () =
  let open Alcotest in
  run "Block_pool" ~and_exit:false
    [
      ("empty", [ test_case "is empty" `Quick test_empty_is_empty ]);
      ( "append_block",
        [
          test_case "appends to both" `Quick test_append_block_appends_to_both;
          test_case "appends twice is noop" `Quick
            test_double_append_block_is_noop;
          test_case "appends with dangling vote" `Quick
            test_append_block_with_dangling_votes;
          test_case "append two different blocks" `Quick
            test_appends_two_different_blocks;
        ] );
      ("remove", [ test_case "simply remove" `Quick test_remove_block ]);
    ]
