open Deku_crypto
open Deku_concepts
open Deku_consensus
open Consensus

(* let pp_action action =
   match action with
   | Consensus_timeout {from} -> Format.eprintf "Consensus timeout: %f\n%!" (Timestamp.to_float from)
   | Consensus_produce _ -> Format.eprintf "Consensus produce\n%!"
   | Consensus_vote {level;_} -> Format.eprintf "Consensus vote: %d\n%!" (Level.to_n level |> Deku_helpers.N.to_z |> Z.to_int) *)

let _pool blocks =
  let open Block in
  let open Block_pool in
  let pool = empty in
  List.fold_left
    (fun pool (block, votes) ->
      let (Block { level; _ }) = block in
      let pool = append_block ~block pool in
      List.fold_left (fun pool vote -> append_vote ~level ~vote pool) pool votes)
    pool blocks

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
  Block.produce ~identity ~level ~previous ~operations

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

  ensure "initial.identity = identity" (initial.identity = identity);
  ensure "initial.validators = validators" (initial.validators = validators);
  (* TODO: This should fail and need block_a as well *)
  (* ensure "initial.block_pool = []" (initial.block_pool = Block_pool.empty); *)
  ensure "initial.state = propose genesis block"
    (initial.state = Propose { finalized = Genesis.block });
  ensure "initial.accepted_at = Timestamp.genesis"
    (initial.accepted_at = Timestamp.genesis)

let test_new_block_on_initial () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~identity ~validators in

  let block = make_block ~identity Genesis.block in
  let Consensus with_block, _ =
    incoming_block ~current:(time 0.1) ~block consensus
  in

  ensure "with_block.identity = identity" (with_block.identity = identity);
  ensure "with_block.validators = validators"
    (with_block.validators = validators);
  (* TODO: This should fail and need block_a as well *)
  (* ensure "with_block.block_pool = [(block, [])]"
     (with_block.block_pool = pool [ (block, []) ] []); *)
  ensure "with_block.state = Vote {finalized = block}"
    (with_block.state = Vote { finalized = block });
  ensure "with_block.accepted_at = 0.1 "
    (with_block.accepted_at = time Deku_constants.genesis_time)

let test_new_vote_on_initial () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash; level; _ } as block) =
    make_block ~identity Genesis.block
  in
  let vote = make_vote ~hash identity in
  let Consensus with_vote, actions =
    incoming_vote ~level ~current:(time 0.1) ~vote consensus
  in
  assert (actions = []);
  ensure "with_vote.identity = identity" (with_vote.identity = identity);
  ensure "with_vote.validators = validators" (with_vote.validators = validators);
  (* TODO: This should fail and need block_a as well *)
  (* ensure "with_vote.block_pool = [(Only_hash block, [self])]"
     (with_vote.block_pool = pool [] [ (hash, Identity.key_hash identity) ]); *)
  ensure "with_vote.state = Apply {pending = block}"
    (with_vote.state = Apply { pending = block });
  ensure "with_vote.accepted_at = " (with_vote.accepted_at = time 0.1)

let test_vote_then_block_on_initial () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash; previous; _ } as block) =
    make_block ~identity Genesis.block
  in
  let vote = make_vote ~hash identity in
  let with_vote, actions =
    incoming_vote ~level ~current:(time 0.1) ~vote consensus
  in
  assert (actions = []);

  let Consensus accepted_block, actions =
    incoming_block ~current:(time 0.2) ~block with_vote
  in

  ensure "accepted_block.identity = identity"
    (accepted_block.identity = identity);
  ensure "accepted_block.validators = validators"
    (accepted_block.validators = validators);
  (* TODO: This should fail and need block_a as well *)
  (* ensure "accepted_block.block_pool = []"
     (accepted_block.block_pool = Block_pool.empty); *)
  ensure "accepted_block.state = Apply { pending = block }"
    (accepted_block.state = Apply { pending = block });
  match actions with
  | [ Consensus_apply { block = applied_block; _ } ] ->
      ensure "accepted_block = block" (applied_block = block)
  (* TODO: check proper votes *)
  (* ensure "votes = vote block b" (match Verified_signature.Set.to_seq votes |> List.of_seq with |hd::[] -> let (Block {hash;_}) = block in   Verified_signature.signed_hash hd = hash) *)
  | _ -> ensure "actions = [accepted]" false

let test_signable_block () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash = hash_a; level; _ } as block_a) =
    make_block ~identity Genesis.block
  in
  let vote = make_vote ~hash:hash_a identity in
  let consensus, _ = incoming_vote ~level ~current:(time 0.1) ~vote consensus in

  let consensus, _ =
    incoming_block ~current:(time 0.2) ~block:block_a consensus
  in

  let (Block { hash = hash_b; level = level_b; _ } as block_b) =
    make_block ~identity block_a
  in
  let Consensus after_voting, actions =
    incoming_block ~current:(time 0.3) ~block:block_b consensus
  in

  ensure "after_voting.identity = identity" (after_voting.identity = identity);
  ensure "after_voting.validators = validators"
    (after_voting.validators = validators);
  (* TODO: Block pool tests later on *)
  (* ensure "after_voting.block_pool = [block_b]"
     (after_voting.block_pool = pool [ (block_b, []) ] []); *)
  ensure "after_voting.state = Apply {pending = block_b}"
    (after_voting.state = Vote { finalized = block_a });
  let () =
    match actions with
    | [ Consensus_vote { level; vote } ] ->
        ensure "vote.hash = block_b"
          (Verified_signature.signed_hash vote = Block_hash.to_blake2b hash_b);
        ensure "vote.key = identity.key"
          (Verified_signature.key vote = Identity.key identity);
        ensure "vote.level = block_b.level" (level = level_b)
    | _ -> ensure "actions != [vote]" false
  in
  ensure "after_voting.accepted_at = 0.1" (after_voting.accepted_at = time 0.1)

let test_fast_forwarding () =
  let identity, _identities, validators = make_validators 1 in
  (* S : Propose *)
  let consensus = Consensus.make ~identity ~validators in

  let block_a = make_block ~identity Genesis.block in
  (* S : Vote , A : Empty *)
  let consensus, _actions1 =
    incoming_block ~current:(time 0.1) ~block:block_a ~prevent_self_sign:true
      consensus
  in
  (* S : Vote, A : Empty *)
  let (Block { hash = hash_b; level = level_b; _ } as block_b) =
    make_block ~identity block_a
  in
  let consensus, _actions2 =
    incoming_block ~current:(time 0.2) ~block:block_b consensus
  in
  let vote = make_vote ~hash:hash_b identity in
  (* S : Vote, A : Empty *)
  let Consensus after_skip, _actions3 =
    incoming_vote ~level:level_b ~current:(time 0.3) ~vote consensus
  in

  ensure "after_skip.identity = identity" (after_skip.identity = identity);
  ensure "after_skip.validators = validators"
    (after_skip.validators = validators);
  (* TODO: Block pool tests later on *)
  (* ensure "after_voting.block_pool = []"
     (after_skip.block_pool = Block_pool.empty); *)
  ensure "after_skip.state = Vote { finalized = block_b }"
    (after_skip.state = Vote { finalized = block_b })
(* match (actions3 @ actions2 @ actions1) with
     | [
      Consensus_timeout { from
   = timeout_b };
      Consensus_accepted_block { block = accepted_a };
      Consensus_trigger_timeout { level = timeout_b };
      Consensus_accepted_block { block = accepted_b };
     ] ->
         ensure "timeout_a = level_a" (timeout_a = level_a);
         ensure "accepted_a = block_b" (accepted_a = block_a);
         ensure "timeout_b = level_b" (timeout_b = level_b);
         ensure "accepted_b = block_b" (accepted_b = block_b)
     | _ -> ensure "actions = [trigger_b; accepted_b; trigger_a; accepted_a]" false
*)

let test_missing_block () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~identity ~validators in

  let block_a = make_block ~identity Genesis.block in

  let (Block { hash = hash_b; level = level_b; _ } as block_b) =
    make_block ~identity block_a
  in
  let consensus, _actions =
    incoming_block ~current:(time 0.2) ~block:block_b consensus
  in

  let vote = make_vote ~hash:hash_b identity in
  let _validator = Identity.key_hash identity in
  let (Consensus after_b as consensus), actions =
    incoming_vote ~level:level_b ~current:(time 0.3) ~vote consensus
  in
  ensure "after_b.identity = identity" (after_b.identity = identity);
  ensure "after_b.validators = validators" (after_b.validators = validators);
  (* TODO: Block pool later *)
  (* ensure "after_b.block_pool = [(block_b, [vote])]"
     (after_b.block_pool = pool [ (block_b, [ validator ]) ] []); *)
  let _ =
    match actions with
    | [ Consensus_request { above } ] ->
        ensure "Consensus_request { above } = level_b" (above = level_b)
    | _ -> ensure "actions = [request]" false
  in
  let Consensus after_a, _actions =
    incoming_block ~current:(time 0.4) ~block:block_a consensus
  in

  (* TODO: this is a bug last_update should be when the last block was singed *)
  (* ensure "after_a.last_update = Some 0.2" (after_a.last_update = Some (time 0.2)); *)
  ensure "after_a.identity = identity" (after_a.identity = identity);
  ensure "after_a.validators = validators" (after_a.validators = validators)
(* TODO: Block pool tests *)
(* (after_a.accepted = Block_hash.Set.of_list [ hash_b; hash_a; previous_a ]);
   ensure "after_a.block_pool = []" (after_a.block_pool = Block_pool.empty); *)
(* TODO: this is probably a bug, *)
(* TODO: action tests *)
(* match actions with
   | [
    Consensus_trigger_timeout { level = timeout_a };
    Consensus_accepted_block { block = accepted_a };
    Consensus_trigger_timeout { level = timeout_b };
    Consensus_accepted_block { block = accepted_b };
   ] ->
       ensure "timeout_a = level_a" (timeout_a = level_a);
       ensure "accepted_a = block_b" (accepted_a = block_a);
       ensure "timeout_b = level_b" (timeout_b = level_b);
       ensure "accepted_b = block_b" (accepted_b = block_b)
   | _ -> ensure "actions = [trigger_b; accepted_b; trigger_a; accepted_a]" false *)

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
