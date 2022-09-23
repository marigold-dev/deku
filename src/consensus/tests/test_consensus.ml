open Deku_crypto
open Deku_concepts
open Deku_consensus
open Consensus

let pool blocks votes =
  let open Block in
  let open Block_pool in
  let pool = empty in
  let pool =
    List.fold_left
      (fun pool (block, votes) ->
        let (Block { hash; _ }) = block in
        let pool = append_block ~block pool in
        List.fold_left
          (fun pool vote -> append_vote ~vote ~hash pool)
          pool votes)
      pool blocks
  in
  let pool =
    List.fold_left
      (fun pool (hash, vote) -> append_vote ~vote ~hash pool)
      pool votes
  in
  pool

let time second = Timestamp.of_float second
let ensure msg actual = Alcotest.(check' bool) ~msg ~expected:true ~actual

let make_identity () =
  let secret = Ed25519.Secret.generate () in
  let secret = Secret.Ed25519 secret in
  Identity.make secret

let make_block ~identity previous =
  let open Block in
  let (Block { hash = previous; level = previous_level; _ }) = previous in
  let level = Level.next previous_level in
  let operations = [] in
  let withdrawal_handles_hash = Deku_crypto.BLAKE2b.hash "tuturu" in
  Block.produce ~parallel_map:List.map ~identity ~level ~previous ~operations
    ~tezos_operations:[] ~withdrawal_handles_hash

let make_validators n =
  assert (n > 0);
  let identities = List.init n (fun _ -> make_identity ()) in
  let self = List.nth identities 0 in
  let validators =
    List.map (fun identity -> Identity.key_hash identity) identities
  in
  let validators = Validators.of_key_hash_list validators in
  (self, identities, validators)

let make_vote ~hash identity =
  let hash = Block_hash.to_blake2b hash in
  Verified_signature.sign hash identity

(* let make_votes ~hash identities =
   List.map (fun identity -> make_vote ~hash identity) identities *)

let test_initial_consensus () =
  let identity, _identities, validators = make_validators 4 in
  let (Consensus initial) = Consensus.make ~identity ~validators in

  ensure "initial.current_block = Genesis.block"
    (initial.current_block = Genesis.block);
  ensure "initial.last_update = 0." (initial.last_update = Timestamp.zero);
  ensure "initial.identity = identity" (initial.identity = identity);
  ensure "initial.validators = validators" (initial.validators = validators);
  ensure "initial.accepted = []" (initial.accepted = Block_hash.Set.empty);
  ensure "initial.block_pool = []" (initial.block_pool = Block_pool.empty)

let test_new_block_on_initial () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~identity ~validators in

  let block = make_block ~identity Genesis.block in
  let Consensus with_block, actions =
    incoming_block ~current:(time 0.0) ~block consensus
  in
  assert (actions = []);

  ensure "with_block.current_block = Genesis.block"
    (with_block.current_block = Genesis.block);
  ensure "with_block.last_update = 0" (with_block.last_update = Timestamp.zero);
  ensure "with_block.identity = identity" (with_block.identity = identity);
  ensure "with_block.validators = validators"
    (with_block.validators = validators);
  ensure "with_block.accepted = []" (with_block.accepted = Block_hash.Set.empty);
  ensure "with_block.block_pool = [(block, [])]"
    (with_block.block_pool = pool [ (block, []) ] [])

let test_new_vote_on_initial () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash; _ }) = make_block ~identity Genesis.block in
  let vote = make_vote ~hash identity in
  let Consensus with_vote, actions =
    incoming_vote ~current:(time 0.0) ~vote consensus
  in
  assert (actions = []);
  ensure "with_vote.current_block = Genesis.block"
    (with_vote.current_block = Genesis.block);
  ensure "with_vote.last_update = 0" (with_vote.last_update = Timestamp.zero);
  ensure "with_vote.identity = identity" (with_vote.identity = identity);
  ensure "with_vote.validators = validators" (with_vote.validators = validators);
  ensure "with_vote.accepted = []" (with_vote.accepted = Block_hash.Set.empty);
  ensure "with_vote.block_pool = [(Only_hash block, [self])]"
    (with_vote.block_pool = pool [] [ (hash, vote) ])

let test_vote_then_block_on_initial () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash; previous; level; _ } as block) =
    make_block ~identity Genesis.block
  in
  let vote = make_vote ~hash identity in
  let with_vote, actions = incoming_vote ~current:(time 0.0) ~vote consensus in
  assert (actions = []);

  let Consensus accepted_block, actions =
    incoming_block ~current:(time 0.1) ~block with_vote
  in

  ensure "accepted_block.current_block = block"
    (accepted_block.current_block = block);
  ensure "accepted_block.last_update = Some"
    (accepted_block.last_update = time 0.1);
  ensure "accepted_block.identity = identity"
    (accepted_block.identity = identity);
  ensure "accepted_block.validators = validators"
    (accepted_block.validators = validators);
  ensure "accepted_block.accepted = [block; genesis]"
    (accepted_block.accepted = Block_hash.Set.of_list [ hash; previous ]);
  ensure "accepted_block.block_pool = []"
    (accepted_block.block_pool = Block_pool.empty);
  match actions with
  | [
   Consensus_trigger_timeout { level = trigger_level };
   Consensus_accepted_block { block = accepted_block; votes = _ };
  ] ->
      ensure "trigger_level = level" (trigger_level = level);
      ensure "accepted_block = block" (accepted_block = block)
  | _ -> ensure "actions = [trigger; accepted]" false

let test_signable_block () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash = hash_a; previous = previous_a; _ } as block_a) =
    make_block ~identity Genesis.block
  in
  let vote = make_vote ~hash:hash_a identity in
  let consensus, actions = incoming_vote ~current:(time 0.0) ~vote consensus in
  assert (actions = []);

  let consensus, actions =
    incoming_block ~current:(time 0.1) ~block:block_a consensus
  in
  assert (List.length actions = 2);

  let (Block { hash = hash_b; _ } as block_b) = make_block ~identity block_a in
  let Consensus after_voting, actions =
    incoming_block ~current:(time 0.2) ~block:block_b consensus
  in

  ensure "after_voting.current_block = block_a"
    (after_voting.current_block = block_a);
  ensure "after_voting.last_update = Some" (after_voting.last_update = time 0.1);
  ensure "after_voting.identity = identity" (after_voting.identity = identity);
  ensure "after_voting.validators = validators"
    (after_voting.validators = validators);
  ensure "after_voting.accepted = [block_a; genesis]"
    (after_voting.accepted = Block_hash.Set.of_list [ hash_a; previous_a ]);
  ensure "after_voting.block_pool = [block_b]"
    (after_voting.block_pool = pool [ (block_b, []) ] []);

  match actions with
  | [ Consensus_broadcast_vote { vote } ] ->
      ensure "vote.hash = block_b"
        (Verified_signature.signed_hash vote = Block_hash.to_blake2b hash_b);
      ensure "vote.key = identity.key"
        (Verified_signature.key vote = Identity.key identity)
  | _ -> ensure "actions = [vote]" false

let test_fast_forwarding () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash = hash_a; previous = previous_a; level = level_a; _ } as
      block_a) =
    make_block ~identity Genesis.block
  in
  let consensus, actions =
    incoming_block ~current:(time 0.0) ~block:block_a consensus
  in
  assert (List.length actions = 0);

  let (Block { hash = hash_b; level = level_b; _ } as block_b) =
    make_block ~identity block_a
  in
  let consensus, actions =
    incoming_block ~current:(time 0.2) ~block:block_b consensus
  in
  assert (List.length actions = 0);

  let vote = make_vote ~hash:hash_b identity in
  let Consensus after_skip, actions =
    incoming_vote ~current:(time 0.3) ~vote consensus
  in

  ensure "after_skip.current_block = block_b"
    (after_skip.current_block = block_b);
  ensure "after_skip.last_update = Some" (after_skip.last_update = time 0.3);
  ensure "after_skip.identity = identity" (after_skip.identity = identity);
  ensure "after_skip.validators = validators"
    (after_skip.validators = validators);
  ensure "after_voting.accepted = [block_b; block_a; genesis]"
    (after_skip.accepted = Block_hash.Set.of_list [ hash_b; hash_a; previous_a ]);
  ensure "after_voting.block_pool = []"
    (after_skip.block_pool = Block_pool.empty);

  match actions with
  | [
   Consensus_trigger_timeout { level = timeout_a };
   Consensus_accepted_block { block = accepted_a; votes = _ };
   Consensus_trigger_timeout { level = timeout_b };
   Consensus_accepted_block { block = accepted_b; votes = _ };
  ] ->
      ensure "timeout_a = level_a" (timeout_a = level_a);
      ensure "accepted_a = block_b" (accepted_a = block_a);
      ensure "timeout_b = level_b" (timeout_b = level_b);
      ensure "accepted_b = block_b" (accepted_b = block_b)
  | _ -> ensure "actions = [trigger_b; accepted_b; trigger_a; accepted_a]" false

let test_missing_block () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash = hash_a; previous = previous_a; level = level_a; _ } as
      block_a) =
    make_block ~identity Genesis.block
  in

  let (Block { hash = hash_b; level = level_b; _ } as block_b) =
    make_block ~identity block_a
  in
  let consensus, actions =
    incoming_block ~current:(time 0.2) ~block:block_b consensus
  in
  assert (actions = []);

  let vote = make_vote ~hash:hash_b identity in
  let (Consensus after_b as consensus), actions =
    incoming_vote ~current:(time 0.3) ~vote consensus
  in
  (match actions with
  | [ Consensus_request_block { hash } ] ->
      ensure "request.hash = hash_a" (hash = hash_a)
  | _ -> ensure "actions = [requiest]" false);

  ensure "after_b.current_block = genesis"
    (after_b.current_block = Genesis.block);
  ensure "after_b.last_update = 0" (after_b.last_update = Timestamp.zero);
  ensure "after_b.identity = identity" (after_b.identity = identity);
  ensure "after_b.validators = validators" (after_b.validators = validators);
  ensure "after_b.accepted = [block_b; block_a]"
    (after_b.accepted = Block_hash.Set.of_list [ hash_b; hash_a ]);
  ensure "after_b.block_pool = [(block_b, [vote])]"
    (after_b.block_pool = pool [ (block_b, [ vote ]) ] []);

  let Consensus after_a, actions =
    incoming_block ~current:(time 0.4) ~block:block_a consensus
  in

  ensure "after_a.current_block = block_b" (after_a.current_block = block_b);
  (* TODO: this is a bug last_update should be when the last block was singed *)
  (* ensure "after_a.last_update = Some 0.2" (after_a.last_update = Some (time 0.2)); *)
  ensure "after_a.identity = identity" (after_a.identity = identity);
  ensure "after_a.validators = validators" (after_a.validators = validators);
  ensure "after_a.accepted = [block_b; block_a; genesis]"
    (after_a.accepted = Block_hash.Set.of_list [ hash_b; hash_a; previous_a ]);
  ensure "after_a.block_pool = []" (after_a.block_pool = Block_pool.empty);
  (* TODO: this is probably a bug, *)
  match actions with
  | [
   Consensus_trigger_timeout { level = timeout_a };
   Consensus_accepted_block { block = accepted_a; votes = _ };
   Consensus_trigger_timeout { level = timeout_b };
   Consensus_accepted_block { block = accepted_b; votes = _ };
  ] ->
      ensure "timeout_a = level_a" (timeout_a = level_a);
      ensure "accepted_a = block_b" (accepted_a = block_a);
      ensure "timeout_b = level_b" (timeout_b = level_b);
      ensure "accepted_b = block_b" (accepted_b = block_b)
  | _ -> ensure "actions = [trigger_b; accepted_b; trigger_a; accepted_a]" false

let run () =
  let open Alcotest in
  run "Consensus" ~and_exit:false
    [
      ( "simple",
        [
          test_case "initial consensus" `Quick test_initial_consensus;
          test_case "initial block" `Quick test_new_block_on_initial;
          test_case "initial vote" `Quick test_new_vote_on_initial;
          test_case "initial accepted" `Quick test_vote_then_block_on_initial;
          test_case "signable block" `Quick test_signable_block;
          test_case "fast forwarding" `Quick test_fast_forwarding;
          test_case "missing block" `Quick test_missing_block;
        ] );
    ]
