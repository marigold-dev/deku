open Deku_crypto
open Deku_concepts
open Deku_consensus

let rec take i = function
  | [] -> []
  | _ when i = 0 -> []
  | x::xs -> x :: take (i-1) xs

let new_secret () =
  let secret = Ed25519.Secret.generate () in
  Secret.Ed25519 secret

let secrets = [new_secret (); new_secret (); new_secret ()]

(* TODO: redefine proper data structure with validators, signers, producers and secrets for testing *)

(* Validators set can be constructed in a different order than secrets, causing
   trouble when we check we try the next producer *)
let key_hash_of_secret secret =
  let key = Key.of_secret secret in
  Key_hash.of_key key

let key_hashes = List.map key_hash_of_secret secrets
let kh_secrets = List.combine key_hashes secrets

let validators =
  List.map key_hash_of_secret secrets
  |> Validators.of_key_hash_list

let key_hashes = Key_hash.Set.elements (validators :> Key_hash.Set.t)
let secrets = List.map (fun a -> List.assoc a kh_secrets) key_hashes
let identities = List.map Identity.make secrets
let producer = List.nth identities 0

let signers =
  List.map
    (fun secret ->
      let identity = Identity.make secret in
      Signer.make ~identity)
    secrets

let consensus_lvl0 =
  Consensus.make ~validators

let consensus_next ~block consensus =
  let current = Timestamp.of_float (Unix.gettimeofday ()) in
  Consensus.apply_block ~current ~block consensus

let make_block ~consensus ~producer =
  let (Consensus.Consensus { current_level; current_block; _ }) = consensus in
  let level = Level.next current_level in
  let previous = current_block in
  let operations = [] in
  Block.produce ~identity:producer ~level ~previous ~operations

let next_producer ~identities ~next_period ~last_block_update =
  let skip = Timestamp.timeouts_since ~current:next_period ~since:(Timestamp.of_float
      last_block_update) in
  let skip = skip mod (List.length identities) in
  let identity = List.nth identities skip in
  Producer.make ~identity

let next_period ~nb_periods ~last_block_update =
  Timestamp.of_float (last_block_update +. nb_periods *. Deku_constants.block_timeout)

let signatures ~current ~block ~consensus ~signers =
  let sign_block block signer =
    Signer.try_to_sign ~current ~consensus ~block signer
  in
  List.map (sign_block block) signers
  |> List.filter Option.is_some
  |> List.map Option.get

(* Checks if a block signed by the validators at a given consensus level *)
let is_signed block consensus signers =
  let current = Timestamp.of_float (Unix.gettimeofday ()) in
  let signatures = signatures ~current ~block ~consensus ~signers in
  List.length signatures = List.length signers

let verifier_accepts ~verifier ~consensus ~signatures =
  let (_verifier, apply) =
    List.fold_left
      (fun (verifier, _) signature ->
        let Verifier.({ verifier; apply }) =
          Verifier.incoming_signature ~consensus ~signature verifier
        in
        (verifier, apply))
      (verifier, None)
      signatures
  in
  Option.is_some apply

let test_is_signed ~expected ~block ~consensus ~signers () =
  let msg =
    Format.asprintf "block should be signed: %b" expected
  in
  Alcotest.(check' bool) ~msg ~expected ~actual:(is_signed block consensus signers)

(* TODO:
  * improve the use of current/test different block producers
  * the current usage of consensus/current assumes the tests are going to run in the same period;
    consensus and block objects should be recreated in each test *)

(* Checks if a block signed by a consensus correctly points to the consensus current block *)
let test_previous_points block consensus signers () =
  let is_signed = is_signed block consensus signers in
  let Block { previous ; _ } = block in
  let Consensus { current_block; _ } = consensus in
  let has_previous = Block_hash.equal current_block previous in
  Alcotest.(check' bool) ~msg:"" ~expected:true ~actual:(Bool.equal is_signed has_previous)

let block_lvl1 = make_block ~producer ~consensus:consensus_lvl0

let consensus_lvl1 = consensus_next ~block:block_lvl1 consensus_lvl0

let block_lvl2 = make_block ~producer ~consensus:consensus_lvl1

let block_lvl2_bad_author = make_block ~producer:(List.nth identities 1) ~consensus:consensus_lvl1

let consensus_lvl2 = consensus_next ~block:block_lvl2 consensus_lvl1

let test_old_unsigned_block () =
  test_is_signed ~expected:false ~block:block_lvl1 ~consensus:consensus_lvl1 ~signers ()

let test_expected_author () =
  test_is_signed ~expected:true ~block:block_lvl2 ~consensus:consensus_lvl1 ~signers ()

let test_bad_author () =
  test_is_signed ~expected:false ~block:block_lvl2_bad_author ~consensus:consensus_lvl1 ~signers ()

let test_old_signed_block () =
  test_is_signed ~expected:false ~block:block_lvl2 ~consensus:consensus_lvl2 ~signers ()

let test_previous_points_1 () =
  test_previous_points block_lvl1 consensus_lvl1 signers ()

let test_previous_points_2 () =
  test_previous_points block_lvl2 consensus_lvl1 signers ()

let test_previous_points_3 () =
  test_previous_points block_lvl2 consensus_lvl2 signers ()

let test_only_producer ~current ~producer ~consensus () =
  let all_producers = List.map (fun identity -> Producer.make ~identity) identities in
  let all_produced =
    List.map
      (Producer.try_to_produce ~current ~consensus)
      all_producers
  in
  let is_producer_allowed = Producer.try_to_produce ~current ~consensus producer |> Option.is_some
  in
  let number_of_producers = List.filter Option.is_some all_produced |> List.length in
  let is_only_producer = 1 = number_of_producers in
  let msg = Printf.sprintf "there was %d producers for this level" number_of_producers in
  Alcotest.(check' bool) ~msg ~expected:true ~actual:(is_producer_allowed && is_only_producer)

let test_timeout_next_producer ?(nb_periods = 1.) () =
  let Consensus { last_block_update ; _ } = consensus_lvl2 in
  let last_block_update = (Option.get last_block_update :> float) in
  let next_period = next_period ~nb_periods ~last_block_update in
  let next_producer = next_producer ~identities ~next_period ~last_block_update in
  test_only_producer ~current:next_period ~producer:next_producer ~consensus:consensus_lvl2 ()

let test_timeout_validators_rotating () =
  let Consensus { last_block_update ; _ } = consensus_lvl2 in
  let last_block_update = (Option.get last_block_update :> float) in
  let periods = List.mapi (fun i _ -> Float.of_int i) identities in
  List.iter
    (fun period ->
      let period = Timestamp.of_float (period +. last_block_update) in
      let producer = next_producer ~identities ~next_period:period ~last_block_update in
      test_only_producer ~current:period ~consensus:consensus_lvl2 ~producer ())
    periods

let test_verifier_signatures ~block ~consensus ~signatures ~expected () =
  let verifier = Verifier.empty in
  let Verifier.({verifier; _}) = Verifier.incoming_block ~consensus ~block verifier in
  let actual = verifier_accepts ~verifier ~consensus ~signatures in
  let n = List.length signatures in
  let should = if expected then "should" else "shouldn\'t" in
  let msg = Printf.sprintf "the block %s have been accepted with %d signatures" should n in
  Alcotest.(check' bool) ~msg ~expected ~actual

let test_verifier_not_enough_signatures () =
  let block = block_lvl2 in
  let consensus = consensus_lvl1 in
  let current = Timestamp.of_float (Unix.gettimeofday ()) in
  let signatures = signatures ~current ~block ~consensus ~signers in
  let two_thirds = Float.(to_int @@ floor @@ 2. /. 3. *. (of_int @@ List.length signatures)) in
  let signatures = take two_thirds signatures in
  test_verifier_signatures ~block ~consensus ~signatures ~expected:false ()

let test_verifier_enough_signatures () =
  let block = block_lvl2 in
  let consensus = consensus_lvl1 in
  let current = Timestamp.of_float (Unix.gettimeofday ()) in
  let signatures = signatures ~current ~block ~consensus ~signers in
  (* TODO: should we test with anything > 2/3, or assume monotonicity? *)
  let two_thirds_plus_one = Float.(to_int @@ floor @@ 2. /. 3. *. (of_int @@ List.length signatures)) + 1 in
  let signatures = take two_thirds_plus_one signatures in
  test_verifier_signatures ~block ~consensus ~signatures ~expected:true ()

let test_block_author_signs () =
  let current = Timestamp.of_float (Unix.gettimeofday ()) in
  let consensus = consensus_lvl1 in
  let block = make_block ~consensus ~producer in
  let author_signs =
    Signer.try_to_sign ~current ~consensus ~block (Signer.make ~identity:producer)
    |> Option.is_some
  in
  Alcotest.(check' bool) ~msg:"" ~expected:true ~actual:author_signs

let test_skip () =
  let validators = Validators.of_key_hash_list key_hashes in
  let skips = List.init (List.length key_hashes + 1) Fun.id in
  let all_skips =
    List.map
      (fun after ->
        List.map
          (fun skip ->
            Validators.skip ~after ~skip validators)
          skips
        |> Key_hash.Set.of_list)
      key_hashes
  in
  let init_set = Key_hash.Set.of_list key_hashes in
  let intersection = List.fold_left Key_hash.Set.inter init_set all_skips in
  let sets_equal = Key_hash.Set.equal intersection init_set in
  Alcotest.(check' bool) ~msg:"" ~expected:true ~actual:sets_equal

let run () =
  let open Alcotest in
  run "Blocks" ~and_exit:false
    [
      ( "general",
        [
          test_case "old_unsigned_block_ignored" `Quick test_old_unsigned_block;
          test_case "old_signed_block_ignored" `Quick test_old_signed_block;
          test_case "expected_author_block_signed" `Quick test_expected_author;
          test_case "bad_author_block_ignored" `Quick test_bad_author;
          test_case "previous_hash_1" `Quick test_previous_points_1;
          test_case "previous_hash_2" `Quick test_previous_points_2;
          test_case "previous_hash_3" `Quick test_previous_points_3;
          test_case "timeout_next_producer" `Quick test_timeout_next_producer;
          test_case "timeout_validators_rotating" `Quick test_timeout_validators_rotating;
          test_case "verifier_not_enough_signatures" `Quick test_verifier_not_enough_signatures;
          test_case "verifier_enough_signatures" `Quick test_verifier_enough_signatures;
          test_case "author_signs_own_blocks" `Quick test_block_author_signs;
          test_case "test_skip" `Quick test_skip;
        ] );
    ]
