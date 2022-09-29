open Deku_crypto
open Deku_concepts
open Deku_consensus
open Consensus

(* TODO: Add action tests *)
(* TODO: Tests for message ordering *)
(* 0 -> 1 -> V1 -> 2 *)
(* 0 -> 1 -> 2 -> V1 *)
(* 0 -> 2 -> 1 -> V1 *)
(* 0 -> 2 -> V2 -> V1 *)
(* 0 -> 2 -> V2 -> 1 *)
(* 0 -> 2 -> 1 -> V2 -> V1 *)

let _level_from_block block =
  let (Block.Block { level; _ }) = block in
  level

let level_to_int level = Level.to_n level |> Deku_stdlib.N.to_z |> Z.to_int

let accepted_level consensus =
  let (Consensus { state; _ }) = consensus in
  match state with
  | Propose { finalized = accepted }
  | Vote { finalized = accepted }
  | Apply { pending = accepted }
  | Corrupted_stuck { finalized = accepted; clash = _ }
  | Corrupted_apply { pending = accepted; clash = _ } ->
      let (Block { level = accepted_level; _ }) = accepted in
      accepted_level
  | Pending_missing { finalized = _; accepted }
  | Pending_apply { pending = _; accepted } ->
      accepted

let pp_action action depth =
  let depth = String.make (depth * 4) ' ' in
  match action with
  | Consensus_timeout { from } ->
      Format.eprintf "%sConsensus timeout: %f\n%!" depth
        (Timestamp.to_float from)
  | Consensus_produce _ -> Format.eprintf "%sConsensus produce\n%!" depth
  | Consensus_vote { level; _ } ->
      Format.eprintf "%sConsensus vote: %d\n%!" depth
        (Level.to_n level |> Deku_stdlib.N.to_z |> Z.to_int)
  | Consensus_apply _ -> Format.eprintf "%sConsensus apply \n%!" depth
  | Consensus_request { above } ->
      Format.eprintf "%sConsensus request %d\n%!" depth
        (Level.to_n above |> Deku_stdlib.N.to_z |> Z.to_int)

let pp_state state depth =
  let depth = String.make (depth * 4) ' ' in
  match state with
  | Propose _ -> Format.eprintf "%sPropose\n%!" depth
  | Vote { finalized } ->
      Format.eprintf "%sVote on block w/ level %d\n%!" depth
        (level_to_int
           (let (Block { level; _ }) = finalized in
            Level.next level))
  | Apply _ -> Format.eprintf "%sApply\n%!" depth
  | Pending_missing _ -> Format.eprintf "%sPending missing\n%!" depth
  | Pending_apply _ -> Format.eprintf "%sPending missing\n%!" depth
  | Corrupted_stuck _ -> Format.eprintf "%sCorrupted stuck\n%!" depth
  | Corrupted_apply _ -> Format.eprintf "%sCorrupted apply\n%!" depth

let pp_state_action (Consensus consensus) actions index =
  Format.eprintf "Index: %d\n%!" index;
  Format.eprintf "Level: %d\n%!"
    (accepted_level (Consensus consensus) |> level_to_int);
  Format.eprintf "State:\n%!";
  pp_state consensus.state 1;
  Format.eprintf "Actions:\n%!";
  List.iter (fun action -> pp_action action 1) actions

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

  let (Block { level; _ } as block) = make_block ~identity Genesis.block in
  let Consensus with_block, actions =
    incoming_block ~current:(time 0.1) ~block consensus
  in

  ensure "with_block.identity = identity" (with_block.identity = identity);
  ensure "with_block.validators = validators"
    (with_block.validators = validators);
  (* TODO: This should fail and need block_a as well *)
  (* ensure "with_block.block_pool = [(block, [])]"
     (with_block.block_pool = pool [ (block, []) ] []); *)
  Format.eprintf "Expected with_block.actions = [], but received ";
  List.iter (fun action -> pp_action action 1) actions;
  Format.eprintf
    "Expected with_block.state = Vote { finalized = block} (level : %d ), but \
     received"
    (level_to_int level);
  pp_state with_block.state 1;
  ensure "with_block.state = Vote {finalized = block}"
    (with_block.state = Vote { finalized = Genesis.block });
  ensure "with_block.accepted_at = 0.1 "
    (with_block.accepted_at = time Deku_constants.genesis_time)

let test_new_vote_on_initial () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash; level; _ }) = make_block ~identity Genesis.block in
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
  Format.eprintf "Expected with_block.actions = [], but received ";
  List.iter (fun action -> pp_action action 1) actions;
  Format.eprintf "\n%!";
  Format.eprintf
    "Expected with_block.state = Vote { finalized = block } (level : %d ), but \
     received\n\
     %!"
    0;
  pp_state with_vote.state 1;
  ensure "with_vote.state = Propose { finalized = Genesis.block }"
    (with_vote.state = Propose { finalized = Genesis.block });
  ensure "with_vote.accepted_at = 0.0" (with_vote.accepted_at = time 0.0)

let test_vote_then_block_on_initial () =
  let identity, _identities, validators = make_validators 1 in
  (* S : Propose genesis *)
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash; level; _ } as block) =
    make_block ~identity Genesis.block
  in
  let vote = make_vote ~hash identity in
  (* S : Propose genesis *)
  let with_vote, actions =
    incoming_vote ~level ~current:(time 0.1) ~vote consensus
  in
  assert (actions = []);

  (* S : Vote genesis *)
  let Consensus accepted_block, actions =
    incoming_block ~current:(time 0.2) ~block with_vote ~prevent_self_sign:true
  in

  ensure "accepted_block.identity = identity"
    (accepted_block.identity = identity);
  ensure "accepted_block.validators = validators"
    (accepted_block.validators = validators);
  (* TODO: This should fail and need block_a as well *)
  (* ensure "accepted_block.block_pool = []"
     (accepted_block.block_pool = Block_pool.empty); *)
  ensure "accepted_block.state = Vote { finalized = Genesis.block }"
    (accepted_block.state = Apply { pending = block });
  ensure "accepted_block.accepted_at = 0.2"
    (accepted_block.accepted_at = time 0.2);
  match actions with
  | [ Consensus_timeout { from }; Consensus_apply { block = applied_block; _ } ]
    ->
      ensure "Consensus_timeout { from = time 0.2 }" (from = time 0.2);
      ensure "Consensus_apply { block = applied_block }" (applied_block = block)
  | _ -> ensure "actions = [accepted]" false

(* Prevent self signing on receiving block 3 *)
let test_signable_block_1 () =
  let identity, _identities, validators = make_validators 1 in
  (* S : Propose Genesis *)
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash = hash_a; level; _ } as block_a) =
    make_block ~identity Genesis.block
  in
  let vote = make_vote ~hash:hash_a identity in
  (* S : Propose Genesis A : Vote *)
  let _consensus, _actions =
    incoming_vote ~level ~current:(time 0.1) ~vote consensus
  in
  pp_state_action _consensus _actions 1;
  (* S : Apply block_a A : [Timeout .2; Consensus_apply (block_a, votes)] *)
  let _consensus, _actions =
    incoming_block ~current:(time 0.2) ~block:block_a ~prevent_self_sign:true
      _consensus
  in
  pp_state_action _consensus _actions 2;

  (* S : Propose block_a A : Consensus produce *)
  let _consensus, _actions =
    match finished ~current:(time 0.3) ~block:block_a _consensus with
    | Ok stuff -> stuff
    | Error _ -> failwith "bad stuff happen on line 216\n%!"
  in
  pp_state_action _consensus _actions 3;

  let block_b = make_block ~identity block_a in

  (* S : Propose A : Pending_apply { pending = block_b ; accepted = level_b?} *)
  let _consensus, _actions =
    incoming_block ~current:(time 0.4) ~block:block_b ~prevent_self_sign:true
      _consensus
  in
  pp_state_action _consensus _actions 4;

  let (Consensus _consensus) = _consensus in
  ensure "after_voting.identity = identity" (_consensus.identity = identity);
  ensure "after_voting.validators = validators"
    (_consensus.validators = validators);
  ensure
    "after_voting.state = Pending_apply { pending = block_b; accepted = \
     level_b }"
    (_consensus.state = Propose { finalized = block_a });
  ensure "actions = []" (_actions = []);
  ensure "after_voting.accepted_at = 0.2" (_consensus.accepted_at = time 0.2)

let test_signable_block_2 () =
  let identity, _identities, validators = make_validators 1 in
  (* S : Propose Genesis *)
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash = hash_a; level; _ } as block_a) =
    make_block ~identity Genesis.block
  in
  let vote = make_vote ~hash:hash_a identity in
  (* S : Propose Genesis *)
  let _consensus, _actions =
    incoming_vote ~level ~current:(time 0.1) ~vote consensus
  in
  pp_state_action _consensus _actions 1;
  (* S : Apply block_a A : [Timeout .2; Consensus_apply (block_a, votes)] *)
  let _consensus, _actions =
    incoming_block ~current:(time 0.2) ~block:block_a ~prevent_self_sign:true
      _consensus
  in
  pp_state_action _consensus _actions 2;

  (* S : Propose block_a A : Consensus produce *)
  let _consensus, _actions =
    match finished ~current:(time 0.3) ~block:block_a _consensus with
    | Ok stuff -> stuff
    | Error _ -> failwith "bad stuff happen on line 216\n%!"
  in
  pp_state_action _consensus _actions 3;

  let (Block { level = level_b; hash = hash_b; _ } as block_b) =
    make_block ~identity block_a
  in

  (* S : Vote A : Consensus Vote *)
  let _consensus, _actions =
    incoming_block ~current:(time 0.4) ~block:block_b _consensus
  in
  pp_state_action _consensus _actions 4;

  let (Consensus _consensus) = _consensus in
  ensure "after_voting.identity = identity" (_consensus.identity = identity);
  ensure "after_voting.validators = validators"
    (_consensus.validators = validators);
  ensure
    "after_voting.state = Pending_apply { pending = block_b; accepted = \
     level_b }"
    (_consensus.state = Vote { finalized = block_a });
  let () =
    match _actions with
    | [ Consensus_vote { level; vote } ] ->
        ensure "level is at level_b" (level = level_b);
        ensure "vote is for block_b"
          (Verified_signature.signed_hash vote = Block_hash.to_blake2b hash_b)
    | _ -> ensure "actions == [Consensus_produce]" false
  in
  ensure "after_voting.accepted_at = 0.2" (_consensus.accepted_at = time 0.2)

let test_fast_forwarding () =
  let identity, _identities, validators = make_validators 1 in
  (* S : Propose *)
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash = hash_a; level = level_a; _ } as block_a) =
    make_block ~identity Genesis.block
  in
  let (Block { hash = hash_b; level = level_b; _ } as block_b) =
    make_block ~identity block_a
  in
  (* S : Propose, A : Empty *)
  let consensus, _actions1 =
    incoming_block ~current:(time 0.1) ~block:block_b ~prevent_self_sign:true
      consensus
  in
  pp_state_action consensus _actions1 1;

  (* S : propose, A : Empty *)
  let consensus, _actions2 =
    incoming_block ~current:(time 0.2) ~block:block_a consensus
      ~prevent_self_sign:true
  in
  pp_state_action consensus _actions2 2;

  let vote = make_vote ~hash:hash_b identity in
  (* S : Pending missing, A : [Timeout; Request] *)
  let Consensus after_skip, _actions3 =
    incoming_vote ~level:level_b ~current:(time 0.3) ~vote consensus
  in
  pp_state_action (Consensus after_skip) _actions3 3;

  let vote = make_vote ~hash:hash_a identity in
  (* S : Pending missing, A : [Consensus apply] *)
  let Consensus after_catch_up, _actions4 =
    incoming_vote ~current:(time 0.4) ~level:level_a ~vote
      (Consensus after_skip)
  in
  pp_state_action (Consensus after_catch_up) _actions4 4;
  ensure "after_skip.identity = identity" (after_skip.identity = identity);
  ensure "after_skip.validators = validators"
    (after_skip.validators = validators);
  ensure
    "after_skip.state = Pending_missing { finalized = Genesis.block; accepted \
     = Level (zero |> next |> next) }"
    (after_skip.state
    = Pending_missing
        { finalized = Genesis.block; accepted = Level.(zero |> next |> next) });
  ensure
    "after_catch_up.state = Pending_apply { pending = block_a; accepted = \
     level_a}"
    (after_catch_up.state
    = Pending_apply { pending = block_a; accepted = level_a })
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
      ~prevent_self_sign:true
  in

  let vote = make_vote ~hash:hash_b identity in
  let _validator = Identity.key_hash identity in
  let (Consensus after_b as consensus), actions =
    incoming_vote ~level:level_b ~current:(time 0.3) ~vote consensus
  in
  ensure "after_b.identity = identity" (after_b.identity = identity);
  ensure "after_b.validators = validators" (after_b.validators = validators);
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

let test_reverse_ordering_2 () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~identity ~validators in

  let (Block { hash = hash_1; level = level_1; _ } as block_1) =
    make_block ~identity Genesis.block
  in
  let vote_1 = make_vote ~hash:hash_1 identity in

  let (Block { hash = hash_2; level = level_2; _ } as block_2) =
    make_block ~identity block_1
  in
  let vote_2 = make_vote ~hash:hash_2 identity in
  let _consensus1, _actions1 =
    incoming_block ~current:(time 0.1) ~block:block_2 consensus
      ~prevent_self_sign:true
  in
  let _consensus2, _actions2 =
    incoming_vote ~current:(time 0.2) ~level:level_2 ~vote:vote_2 _consensus1
  in

  let _consensus3, _actions3 =
    incoming_block ~current:(time 0.3) ~block:block_1 _consensus2
      ~prevent_self_sign:true
  in
  let _consensus4, _actions4 =
    incoming_vote ~current:(time 0.4) ~level:level_1 ~vote:vote_1 _consensus3
  in
  let _consensus5, _actions5 =
    match finished ~current:(time 0.5) ~block:block_1 _consensus4 with
    | Ok (consensus, actions) -> (consensus, actions)
    | Error `No_pending_block -> failwith "no pending block\n%!"
    | Error `Wrong_pending_block -> failwith "wrong pending block\n%!"
  in
  let Consensus _consensus6, _actions6 =
    match finished ~current:(time 0.6) ~block:block_2 _consensus5 with
    | Ok (consensus, actions) -> (consensus, actions)
    | Error `No_pending_block -> failwith "no pending block\n%!"
    | Error `Wrong_pending_block -> failwith "wrong pending block\n%!"
  in
  ensure "_consensus.identity = identity" (_consensus6.identity = identity);
  ensure "_consensus.validators = validators"
    (_consensus6.validators = validators);
  pp_state_action _consensus1 _actions1 1;
  pp_state_action _consensus2 _actions2 2;
  pp_state_action _consensus3 _actions3 3;
  pp_state_action _consensus4 _actions4 4;
  pp_state_action _consensus5 _actions5 5;
  pp_state_action (Consensus _consensus6) _actions6 6;
  ensure "_consensus.state = Apply { pending = block_1 }"
    (_consensus6.state = Apply { pending = block_1 });
  ()

let run () =
  let open Alcotest in
  run "Consensus" ~and_exit:false
    [
      ( "simple",
        [
          test_case "initial consensus" `Quick test_initial_consensus;
          test_case "new block on initial" `Quick test_new_block_on_initial;
          test_case "new vote on initial" `Quick test_new_vote_on_initial;
          test_case "vote then block on initial" `Quick
            test_vote_then_block_on_initial;
          test_case "signable block 1" `Quick test_signable_block_1;
          test_case "signable block 2" `Quick test_signable_block_2;
          test_case "fast forwarding" `Quick test_fast_forwarding;
          test_case "missing block" `Quick test_missing_block;
          test_case "reverse ordering" `Quick test_reverse_ordering_2;
        ] );
    ]
