(*
 This code is designed to wrap the following interface (basically all the internal Deku logic)
   Process handlers in chain.ml:

      val make :
        identity:Identity.t ->
        validators:Key_hash.t list ->
        pool:Parallel.Pool.t ->
        chain

      val incoming :
        raw_expected_hash:string ->
        raw_content:string ->
        chain ->
        chain * fragment option
      (** [incoming ~raw_expected_hash ~raw_content chain] *)

      val timeout : current:Timestamp.t -> chain -> fragment option
      (** [incoming_timeout ~current chain] *)

      val apply :
        current:Timestamp.t -> outcome:outcome -> chain -> chain * action list
      (** [apply ~current ~outcome chain ]*)

      val compute : fragment -> outcome
      (** [compute fragment] Can be executed in parallel *)

  You have the same powers as an adversary in the partially synchronous model. You can split the network however you like.
      Tests for chain:
          - Sending block 2 before block 1
          - Accepting block 2 before block 1
          - Receiving votes for block b before block is received
          - Trigger timeout at 1 after receiving block 2
          - Shuffling message order

*)

open Chain_genesis
open Chain_messages
open Deku_chain_run
open Deku_chain
open Deku_consensus
open Deku_concepts

let elements_are_equal = function
  | [] -> true
  | hd :: tl ->
      let rec go = function
        | [] -> true
        | t :: tl -> ( match t = hd with true -> go tl | false -> false)
      in
      go tl

(* TODO: Should we extract much more data about the chain state with this? Then we can run more detailed tests *)
let extract_levels chains_actions_map =
  Map.fold
    (fun _ (chain, _) list ->
      let (Chain.Chain { consensus; _ }) = chain in
      let (Consensus.Consensus { current_block; _ }) = consensus in
      let (Block { level; _ }) = current_block in
      let level = Level.to_n level |> Deku_stdlib.N.to_z |> Z.to_int in
      level :: list)
    chains_actions_map []

let test_run_normally () =
  let output = run chains_actions_map 0 ([], empty_messages, 0) 20 in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:"chain runs normally and progresses for 20 rounds"
    ~expected:(true, true)
    ~actual:(level > 1, levels_are_equal)

let test_run_10_round_random_filter () =
  let output = run chains_actions_map 10 ([], empty_messages, 0) 40 in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:"chain runs with filters for 10 rounds, and recovers in 40 rounds"
    ~expected:(true, true)
    ~actual:(level > 1, levels_are_equal)

let test_run_20_round_random_filter () =
  let output = run chains_actions_map 20 ([], empty_messages, 0) 80 in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:"chain runs with filters for 20 rounds, and recovers in 80 rounds"
    ~expected:(true, true)
    ~actual:(level > 1, levels_are_equal)

let test_run_30_round_random_filter () =
  let output = run chains_actions_map 30 ([], empty_messages, 0) 120 in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:"chain runs with filters for 30 rounds, and recovers in 120 rounds"
    ~expected:(true, true)
    ~actual:(level > 1, levels_are_equal)

let test_run_5_round_message_steal () =
  let output =
    run chains_actions_map 0 ([ List.hd validators ], empty_messages, 5) 40
  in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:
      "chain runs with stolen messages for 5 rounds and progresses after 40 \
       rounds"
    ~expected:(true, true)
    ~actual:(level > 3, levels_are_equal)

let test_run_10_round_message_steal () =
  let output =
    run chains_actions_map 0 ([ List.hd validators ], empty_messages, 10) 120
  in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:
      "chain runs with stolen messsages for 10 rounds and progresses after 120 \
       rounds"
    ~expected:(true, true)
    ~actual:(level > 3, levels_are_equal)

let test_run_15_round_message_steal () =
  let output =
    run chains_actions_map 0 ([ List.hd validators ], empty_messages, 15) 40
  in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:
      "chain runs with stolen messages for 15 rounds and progresses after 40 \
       rounds"
    ~expected:(true, true)
    ~actual:(level > 3, levels_are_equal)

let run () =
  let open Alcotest in
  run "running chain tests" ~and_exit:false
    [
      ("1", [ test_case "" `Slow test_run_normally ]);
      ("2", [ test_case "" `Slow test_run_10_round_random_filter ]);
      ("3", [ test_case "" `Slow test_run_20_round_random_filter ]);
      ("4", [ test_case "" `Slow test_run_30_round_random_filter ]);
      ("5", [ test_case "" `Slow test_run_5_round_message_steal ]);
      ("6", [ test_case "" `Slow test_run_10_round_message_steal ]);
      ("7", [ test_case "" `Slow test_run_15_round_message_steal ]);
    ]

let () = run ()
(* Things we can do:
   [ ] Handle lar ge block sizes
   [X] Filter random messages
   [ ] Filter random groups of messages (multiple filters)
   [ ] DDos a node? (Universal Filter)
   [ ] Send a single set of messages out of order
   [ ] Send random messages out of order
*)
