open Deku_crypto
open Deku_concepts
open Deku_consensus
open Consensus

(* TODO: These tests are verbose, poorly named, complicated, and give no information about the test failure state *)
let time second = Timestamp.of_float second
let next_timeout until = time (until +. Deku_constants.block_timeout)
let ensure msg actual = Alcotest.(check' bool) ~msg ~expected:true ~actual

let make_identity () =
  let secret = Ed25519.Secret.generate () in
  let secret = Secret.Ed25519 secret in
  Identity.make secret

let empty_payload = Payload.Payload []

let make_block ~identity ~previous ~payload ~tezos_operations
    ~withdrawal_handles_hash =
  let open Block in
  let (Block { hash = previous; level = previous_level; _ }) = previous in
  let level = Level.next previous_level in
  Block.produce ~identity ~level ~previous ~payload ~tezos_operations
    ~withdrawal_handles_hash

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

let test_initial_consensus () =
  let _identity, _identities, validators = make_validators 4 in
  let (Consensus initial) = Consensus.make ~validators in

  ensure "initial.validators = validators" (initial.validators = validators);
  (match initial.state with
  | Propose { finalized } ->
      ensure "finalized = Genesis.block" (finalized = Genesis.block)
  | _ -> ensure "initial.state = propose" false);
  ensure "initial.accepted_at = Timestamp.genesis"
    (initial.accepted_at = Timestamp.genesis)

let test_new_block_on_initial () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~validators in
  let block =
    make_block ~identity ~previous:Genesis.block ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in
  let Consensus with_block, _actions =
    incoming_block ~identity ~current:(time 0.1) ~block consensus
  in

  ensure "with_block.validators = validators"
    (with_block.validators = validators);
  (match with_block.state with
  | Vote { finalized } ->
      ensure "finalized = Genesis.block" (finalized = Genesis.block)
  | _ -> ensure "with_block.state = Vote" false);
  ensure "with_block.accepted_at = time Deku_constants.genesis_time"
    (with_block.accepted_at = time Deku_constants.genesis_time)

let test_new_vote_on_initial () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~validators in

  let (Block { hash; level; _ }) =
    make_block ~identity ~previous:Genesis.block ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in
  let vote = make_vote ~hash identity in
  let Consensus with_vote, actions =
    incoming_vote ~level ~current:(time 0.1) ~vote consensus
  in
  assert (actions = []);
  ensure "with_vote.validators = validators" (with_vote.validators = validators);
  (match with_vote.state with
  | Propose { finalized } ->
      ensure "finalized = Genesis.block" (finalized = Genesis.block)
  | _ -> ensure "with_vote.state = Propose" false);
  ensure "with_vote.accepted_at = time 0.0" (with_vote.accepted_at = time 0.0)

let test_vote_then_block_on_initial () =
  let identity, _identities, validators = make_validators 1 in
  (* S : Propose genesis *)
  let consensus = Consensus.make ~validators in

  let (Block { hash; level; _ } as block) =
    make_block ~identity ~previous:Genesis.block ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in
  let vote = make_vote ~hash identity in
  (* S : Propose genesis *)
  let with_vote, actions =
    incoming_vote ~level ~current:(time 0.1) ~vote consensus
  in
  assert (actions = []);

  (* S : Vote genesis *)
  let Consensus accepted_block, actions =
    incoming_block ~identity ~current:(time 0.2) ~block with_vote
  in

  ensure "accepted_block.validators = validators"
    (accepted_block.validators = validators);
  (match accepted_block.state with
  | Apply { pending } ->
      ensure "accepted_block.state = Vote { finalized = Genesis.block }"
        (pending = block)
  | _ -> ensure "accepted_block.state = Vote" false);
  ensure "accepted_block.accepted_at = 0.2"
    (accepted_block.accepted_at = time 0.2);
  match actions with
  | [
   Consensus_timeout { until };
   Consensus_apply { block = applied_block; _ };
   Consensus_vote { level; vote };
  ] ->
      ensure "Consensus_timeout { until = 0.2 }" (until = next_timeout 0.2);
      ensure "Consensus_apply { block = applied_block }" (applied_block = block);
      ensure "Consensus_vote {level = Level.(zero |> next)}"
        (level = Level.(zero |> next));
      ensure "Consensus_Vote {vote = vote}"
        (Deku_concepts.Verified_signature.signed_hash vote
        = Block_hash.to_blake2b hash)
  | _ -> ensure "actions = [accepted]" false

let test_on_2_no_vote () =
  let identity, _identities, validators = make_validators 1 in
  (* S : Propose Genesis *)
  let consensus = Consensus.make ~validators in

  let (Block { hash = hash_1; level; _ } as block_1) =
    make_block ~identity ~previous:Genesis.block ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in
  let _consensus, _actions =
    incoming_block ~identity ~current:(time 0.1) ~block:block_1 consensus
  in

  let vote1 = make_vote ~hash:hash_1 identity in
  let _consensus, _actions2 =
    incoming_vote ~current:(time 0.2) ~level ~vote:vote1 _consensus
  in

  let (Block { hash = hash_2; level = level_2; _ } as block_2) =
    let payload = Payload.Payload [] in
    make_block ~identity ~previous:block_1 ~payload ~tezos_operations:[]
      ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in
  let vote2 = make_vote ~hash:hash_2 identity in
  let _consensus, _actions3 =
    incoming_block ~identity ~current:(time 0.3) ~block:block_2 _consensus
  in

  let (Consensus _consensus_in as _consensus), _actions4 =
    incoming_vote ~current:(time 0.4) ~level:level_2 ~vote:vote2 _consensus
  in

  ensure "_consensus.validators = validators"
    (_consensus_in.validators = validators);
  (match _consensus_in.state with
  | Pending_apply { pending; accepted } ->
      ensure "pending = block_1" (pending = block_1);
      ensure "accepted = level_2" (accepted = level_2)
  | _ -> ensure "_consensus_in.state = Pending_apply" false);
  (match _actions4 with
  | [ Consensus_timeout { until } ] ->
      ensure "Consensus_timeout {until = 0.4}" (until = next_timeout 0.4)
  | _ -> ensure "actions == [Consensus_timeout]" false);
  ensure "_consensus.accepted_at = 0.4" (_consensus_in.accepted_at = time 0.4);

  let (Consensus _consensus_in as _consensus), _actions =
    match finished ~identity ~current:(time 0.5) ~block:block_1 _consensus with
    | Ok stuff -> stuff
    | Error `No_pending_block -> failwith "no pending block"
    | Error `Wrong_pending_block -> failwith "wrong pending block"
  in

  (match _consensus_in.state with
  | Apply { pending } -> ensure "pending = block_2" (pending = block_2)
  | _ -> ensure "pending = Apply" false);

  let (Consensus _consensus_in as _consensus), _actions =
    match finished ~identity ~current:(time 0.6) ~block:block_2 _consensus with
    | Ok stuff -> stuff
    | Error `No_pending_block -> failwith "no pending block"
    | Error `Wrong_pending_block -> failwith "wrong pending block"
  in

  match _consensus_in.state with
  | Propose { finalized } -> ensure "finalized = block_2" (finalized = block_2)
  | _ -> ensure "_consensus_in.state = Propose" false

let test_multi_on_2_no_vote () =
  let _, identitites, validators = make_validators 4 in
  let i1, i2, i3, identity =
    match identitites with
    | [ h1; h2; h3; h4 ] -> (h1, h2, h3, h4)
    | _ -> failwith "something went wrong!"
  in
  let consensus = Consensus.make ~validators in
  let (Block { hash = hash_1; level = level_1; _ } as block_1) =
    make_block ~identity ~previous:Genesis.block ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in

  let votes1 = List.map (fun id -> make_vote ~hash:hash_1 id) [ i1; i2; i3 ] in

  let (Block { hash = hash_2; level = level_2; _ } as block_2) =
    make_block ~identity ~previous:block_1 ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in

  let votes2 = List.map (fun id -> make_vote ~hash:hash_2 id) [ i1; i2; i3 ] in

  let consensus, _actions =
    List.fold_left
      (fun (consensus, _) vote ->
        incoming_vote ~current:(time 0.1) ~level:level_1 ~vote consensus)
      (consensus, []) votes1
  in
  let consensus, _actions =
    incoming_block ~identity ~current:(time 0.2) ~block:block_1 consensus
  in

  let consensus, _actions =
    List.fold_left
      (fun (consensus, _) vote ->
        incoming_vote ~current:(time 0.3) ~level:level_2 ~vote consensus)
      (consensus, []) votes2
  in

  let Consensus consensus_in, _actions =
    incoming_block ~identity ~current:(time 0.4) ~block:block_2 consensus
  in

  ensure "consensus.validators = validators"
    (consensus_in.validators = validators);
  (match consensus_in.state with
  | Pending_apply { pending; accepted } ->
      ensure "pending = block_1" (pending = block_1);
      ensure "accepted = level_2" (accepted = level_2)
  | _ -> ensure "consensus_in.state = Pending_apply" false);
  let _ =
    match _actions with
    | [ Consensus_timeout { until } ] ->
        ensure "until = next_timeout 0.4" (until = next_timeout 0.4)
    | _ -> ensure "actions == stuff" false
  in
  ensure "consensus_in.accepted_at = time 0.4"
    (consensus_in.accepted_at = time 0.4)

let test_signable_block_1 () =
  let identity, _identities, validators = make_validators 1 in
  (* S : Propose Genesis *)
  let consensus = Consensus.make ~validators in

  let (Block { hash = hash_1; level; _ } as block_1) =
    make_block ~identity ~previous:Genesis.block ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in
  let vote = make_vote ~hash:hash_1 identity in
  (* S : Propose Genesis A : Vote *)
  let _consensus, _actions =
    incoming_vote ~level ~current:(time 0.1) ~vote consensus
  in
  (* S : Apply block_a A : [Timeout .2; Consensus_apply (block_a, votes)] *)
  let _consensus, _actions =
    incoming_block ~identity ~current:(time 0.2) ~block:block_1 _consensus
  in

  (* S : Propose block_a A : Consensus produce *)
  let _consensus, _actions =
    match finished ~identity ~current:(time 0.3) ~block:block_1 _consensus with
    | Ok stuff -> stuff
    | Error `No_pending_block -> failwith "no pending block"
    | Error `Wrong_pending_block -> failwith "wrong pending block"
  in

  let (Block { hash = hash_2; level = level_2; _ } as block_2) =
    make_block ~identity ~previous:block_1 ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in

  (* S : Propose A : Pending_apply { pending = block_2 ; accepted = level_2?} *)
  let _consensus, _actions =
    incoming_block ~identity ~current:(time 0.4) ~block:block_2 _consensus
  in

  let (Consensus consensus_in) = _consensus in
  ensure "consensus_in.validators = validators"
    (consensus_in.validators = validators);
  (match consensus_in.state with
  | Vote { finalized } -> ensure "finalized = block_1" (finalized = block_1)
  | _ -> ensure "consensus_in.state = Vote" false);
  (match _actions with
  | [ Consensus_vote { level; vote; _ } ] ->
      ensure "level = level_2" (level = level_2);
      ensure "hash = hash_b"
        (Deku_concepts.Verified_signature.signed_hash vote
        = Block_hash.to_blake2b hash_2)
  | _ -> ensure "actions == [Consensus_vote]" false);
  ensure "after_voting.accepted_at = time 0.2"
    (consensus_in.accepted_at = time 0.2)

let test_signable_block_2 () =
  let identity, _identities, validators = make_validators 1 in
  (* S : Propose Genesis *)
  let consensus = Consensus.make ~validators in

  let (Block { hash = hash_1; level; _ } as block_1) =
    make_block ~identity ~previous:Genesis.block ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in
  let vote = make_vote ~hash:hash_1 identity in
  (* S : Propose Genesis *)
  let _consensus, _actions =
    incoming_vote ~level ~current:(time 0.1) ~vote consensus
  in
  (* S : Apply block_a A : [Timeout .2; Consensus_apply (block_a, votes)] *)
  let _consensus, _actions =
    incoming_block ~identity ~current:(time 0.2) ~block:block_1 _consensus
  in

  (* S : Propose block_a A : Consensus produce *)
  let _consensus, _actions =
    match finished ~identity ~current:(time 0.3) ~block:block_1 _consensus with
    | Ok stuff -> stuff
    | Error `No_pending_block -> failwith "no pending block"
    | Error `Wrong_pending_block -> failwith "wrong pending block"
  in

  let (Block { level = level_2; hash = hash_2; _ } as block_2) =
    make_block ~identity ~previous:block_1 ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in

  (* S : Vote A : Consensus Vote *)
  let _consensus, _actions =
    incoming_block ~identity ~current:(time 0.4) ~block:block_2 _consensus
  in

  let (Consensus consensus_in) = _consensus in
  ensure "after_voting.validators = validators"
    (consensus_in.validators = validators);
  (match consensus_in.state with
  | Vote { finalized } -> ensure "finalized = block_1" (finalized = block_1)
  | _ -> ensure "consensus_in.state = Vote" false);
  let () =
    match _actions with
    | [ Consensus_vote { level; vote } ] ->
        ensure "level = level_2" (level = level_2);
        ensure "vote is for block_2"
          (Verified_signature.signed_hash vote = Block_hash.to_blake2b hash_2)
    | _ -> ensure "actions == [Consensus_vote]" false
  in
  ensure "consensus_in.accepted_at = time 0.2"
    (consensus_in.accepted_at = time 0.2)

let test_fast_forwarding () =
  let identity, _identities, validators = make_validators 1 in
  (* S : Propose *)
  let consensus = Consensus.make ~validators in

  let (Block { hash = hash_1; level = level_1; _ } as block_1) =
    make_block ~identity ~previous:Genesis.block ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in
  let (Block { hash = hash_2; level = level_2; _ } as block_2) =
    make_block ~identity ~previous:block_1 ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in
  (* S : Propose, A : Empty *)
  let consensus, _actions1 =
    incoming_block ~identity ~current:(time 0.1) ~block:block_2 consensus
  in

  (* S : propose, A : Empty *)
  let consensus, _actions2 =
    incoming_block ~identity ~current:(time 0.2) ~block:block_1 consensus
  in

  let vote = make_vote ~hash:hash_2 identity in
  (* S : Pending missing, A : [Timeout; Request] *)
  let (Consensus after_skip_in as after_skip), _actions3 =
    incoming_vote ~level:level_2 ~current:(time 0.3) ~vote consensus
  in

  let vote = make_vote ~hash:hash_1 identity in
  (* S : Pending missing, A : [Consensus apply] *)
  let Consensus after_catch_up_in, _actions4 =
    incoming_vote ~current:(time 0.4) ~level:level_1 ~vote after_skip
  in
  ensure "after_skip.validators = validators"
    (after_skip_in.validators = validators);
  (match after_skip_in.state with
  | Pending_missing { finalized; accepted } ->
      ensure "finalized = Genesis.block" (finalized = Genesis.block);
      ensure "accepted = Level (zero |> next |> next)"
        (accepted = Level.(zero |> next |> next))
  | _ -> ensure "after_skip_in.state = Pending_missing" false);
  match after_catch_up_in.state with
  | Pending_apply { pending; accepted } ->
      ensure "pending = block_1" (pending = block_1);
      ensure "accepted = level_2" (accepted = level_2)
  | _ -> ensure "after_catch_up_in.state = Pending_apply" false

let test_missing_block () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~validators in

  let block_1 =
    make_block ~identity ~previous:Genesis.block ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in

  let (Block { hash = hash_2; level = level_2; _ } as block_2) =
    make_block ~identity ~previous:block_1 ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in
  let consensus, _actions =
    incoming_block ~identity ~current:(time 0.2) ~block:block_2 consensus
  in

  let vote = make_vote ~hash:hash_2 identity in
  let (Consensus after_2 as consensus), actions =
    incoming_vote ~level:level_2 ~current:(time 0.3) ~vote consensus
  in
  ensure "after_2.validators = validators" (after_2.validators = validators);
  let _ =
    match actions with
    | [ Consensus_timeout { until }; Consensus_request { above } ] ->
        ensure "above = Level.zero" (above = Level.zero);
        ensure "until = next_timeout 0.3" (until = next_timeout 0.3)
    | _ -> ensure "actions = [Consensus_timeout; Consensus_request]" false
  in
  let Consensus after_1, _actions =
    incoming_block ~identity ~current:(time 0.4) ~block:block_1 consensus
  in
  ensure "after_1.validators = validators" (after_1.validators = validators)

let test_reverse_ordering_2 () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~validators in

  let (Block { hash = hash_1; level = level_1; _ } as block_1) =
    make_block ~identity ~previous:Genesis.block ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in
  let vote_1 = make_vote ~hash:hash_1 identity in

  let (Block { hash = hash_2; level = level_2; _ } as block_2) =
    make_block ~identity ~previous:block_1 ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in
  let vote_2 = make_vote ~hash:hash_2 identity in
  let _consensus1, _actions1 =
    incoming_block ~identity ~current:(time 0.1) ~block:block_2 consensus
  in

  let _consensus2, _actions2 =
    incoming_vote ~current:(time 0.2) ~level:level_2 ~vote:vote_2 _consensus1
  in

  let _consensus3, _actions3 =
    incoming_block ~identity ~current:(time 0.3) ~block:block_1 _consensus2
  in

  let _consensus4, _actions4 =
    incoming_vote ~current:(time 0.4) ~level:level_1 ~vote:vote_1 _consensus3
  in
  let _consensus5, _actions5 =
    match finished ~identity ~current:(time 0.5) ~block:block_1 _consensus4 with
    | Ok (consensus, actions) -> (consensus, actions)
    | Error `No_pending_block -> failwith "no pending block\n%!"
    | Error `Wrong_pending_block -> failwith "wrong pending block\n%!"
  in
  let Consensus consensus6_in, _actions6 =
    match finished ~identity ~current:(time 0.6) ~block:block_2 _consensus5 with
    | Ok (consensus, actions) -> (consensus, actions)
    | Error `No_pending_block -> failwith "no pending block\n%!"
    | Error `Wrong_pending_block -> failwith "wrong pending block\n%!"
  in
  ensure "consensus6_in.validators = validators"
    (consensus6_in.validators = validators);
  match consensus6_in.state with
  | Propose { finalized } -> ensure "finalized = block_2" (finalized = block_2)
  | _ -> ensure "consensus6_in.state = Propose" false

let test_one_two_V1 () =
  let identity, _identities, validators = make_validators 1 in
  let consensus = Consensus.make ~validators in

  let (Block { hash = hash_1; level = level_1; _ } as block_1) =
    make_block ~identity ~previous:Genesis.block ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in

  let _consensus, _actions =
    incoming_block ~identity ~current:(time 0.1) ~block:block_1 consensus
  in

  let (Block { hash = _hash_2; level = _level_2; _ } as block_2) =
    make_block ~identity ~previous:block_1 ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in

  let _consensus, _actions =
    incoming_block ~identity ~current:(time 0.2) ~block:block_2 _consensus
  in

  let vote_1 = make_vote ~hash:hash_1 identity in

  let Consensus consensus_in, _actions =
    incoming_vote ~level:level_1 ~current:(time 0.3) ~vote:vote_1 _consensus
  in

  ensure "consensus_in.validators = validators"
    (consensus_in.validators = validators);
  (match consensus_in.state with
  | Apply { pending } -> ensure "pending = block_1" (pending = block_1)
  | _ -> ensure "consensus_in.state = Apply" false);
  match _actions with
  | [ Consensus_timeout { until }; Consensus_apply { block; votes } ] ->
      ensure "until = next_timeout 0.3" (until = next_timeout 0.3);
      ensure "block = block_1" (block = block_1);
      ensure "vote = hash"
        (Verified_signature.Set.choose votes
        |> Verified_signature.signed_hash
        = Block_hash.to_blake2b hash_1)
  | _ -> ensure "actions == [Consensus_timeout]" false

let test_two_one_V1 () =
  let identity, _identities, validators = make_validators 1 in
  let _consensus = Consensus.make ~validators in

  let (Block { hash = hash_1; level = level_1; _ } as block_1) =
    make_block ~identity ~previous:Genesis.block ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in

  let (Block { hash = _hash_2; level = _level_2; _ } as block_2) =
    make_block ~identity ~previous:block_1 ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in

  let _consensus, _actions =
    incoming_block ~identity ~current:(time 0.2) ~block:block_2 _consensus
  in

  let _consensus, _actions =
    incoming_block ~identity ~current:(time 0.1) ~block:block_1 _consensus
  in

  let vote_1 = make_vote ~hash:hash_1 identity in

  let Consensus consensus_in, _actions =
    incoming_vote ~level:level_1 ~current:(time 0.3) ~vote:vote_1 _consensus
  in

  ensure "consensus_in.validators = validators"
    (consensus_in.validators = validators);
  (match consensus_in.state with
  | Apply { pending } -> ensure "pending = block_1" (pending = block_1)
  | _ -> ensure "consensus_in.state = Apply" false);
  match _actions with
  | [ Consensus_timeout { until }; Consensus_apply { block; votes } ] ->
      ensure "until = next_timeout 0.3" (until = next_timeout 0.3);
      ensure "block = block_1" (block = block_1);
      ensure "vote = hash"
        (Verified_signature.Set.choose votes
        |> Verified_signature.signed_hash
        = Block_hash.to_blake2b hash_1)
  | _ -> ensure "actions == [Consensus_timeout; Consensus_apply]" false

let test_two_V2_V1 () =
  let identity, _identities, validators = make_validators 1 in
  let _consensus = Consensus.make ~validators in

  let (Block { hash = hash_1; level = level_1; _ } as block_1) =
    make_block ~identity ~previous:Genesis.block ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in

  let (Block { hash = hash_2; level = level_2; _ } as block_2) =
    make_block ~identity ~previous:block_1 ~payload:empty_payload
      ~tezos_operations:[] ~withdrawal_handles_hash:(BLAKE2b.hash "blah")
  in

  let _consensus, _actions =
    incoming_block ~identity ~current:(time 0.1) ~block:block_2 _consensus
  in

  let vote_2 = make_vote ~hash:hash_2 identity in

  let _consensus, _actions =
    incoming_vote ~level:level_2 ~current:(time 0.2) ~vote:vote_2 _consensus
  in

  let vote_1 = make_vote ~hash:hash_1 identity in

  let Consensus consensus_in, _actions =
    incoming_vote ~level:level_1 ~current:(time 0.3) ~vote:vote_1 _consensus
  in

  ensure "consensus_in.validators = validators"
    (consensus_in.validators = validators);
  (match consensus_in.state with
  | Pending_missing { finalized; accepted } ->
      ensure "finalized = Genesis.block" (finalized = Genesis.block);
      ensure "accepted = level_2" (accepted = level_2)
  | _ -> ensure "consensus_in.state = Pending_missing" false);
  match _actions with
  | [] -> ensure "actions == []" true
  | _ -> ensure "actions == []" false

let run () =
  let open Alcotest in
  run "Consensus" ~and_exit:false
    [
      ( "simple",
        [
          test_case "initial consensus" `Quick test_initial_consensus;
          test_case "new block on initial" `Quick test_new_block_on_initial;
          test_case "new vote on initial" `Quick test_new_vote_on_initial;
          test_case "on 2 no vote" `Quick test_on_2_no_vote;
          test_case "multi on 2 no vote" `Quick test_multi_on_2_no_vote;
          test_case "vote then block on initial" `Quick
            test_vote_then_block_on_initial;
          test_case "signable block 1" `Quick test_signable_block_1;
          test_case "signable block 2" `Quick test_signable_block_2;
          test_case "fast forwarding" `Quick test_fast_forwarding;
          test_case "missing block" `Quick test_missing_block;
          test_case "reverse ordering" `Quick test_reverse_ordering_2;
          test_case "one two v1" `Quick test_one_two_V1;
          test_case "two one v1" `Quick test_two_one_V1;
          test_case "two v2 v1" `Quick test_two_V2_V1;
        ] );
    ]
