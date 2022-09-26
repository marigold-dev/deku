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

let cartesian_product l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l)

let elements_are_equal = function
  | [] -> true
  | hd :: tl ->
      let rec go = function
        | [] -> true
        | t :: tl -> ( match t = hd with true -> go tl | false -> false)
      in
      go tl

let extract_levels chains_actions_map =
  Map.fold
    (fun _ (chain, _) list ->
      let (Chain.Chain { consensus; _ }) = chain in
      let (Consensus.Consensus { current_block; _ }) = consensus in
      let (Block { level; _ }) = current_block in
      let level = Level.to_n level |> Deku_stdlib.N.to_z |> Z.to_int in
      level :: list)
    chains_actions_map []

let test_universal_filter_work () =
  let messages =
    let data =
      cartesian_product validators validators
      |> cartesian_product [ Response; Request; Broadcast ]
      |> List.map (fun (e, (e', e'')) -> (e, e', e''))
    in
    List.map
      (fun (kind, to_, from) ->
        Message
          {
            from;
            to_;
            raw_expected_hash = "";
            raw_content = "";
            kind;
            id = Deku_gossip.Request_id.initial;
          })
      data
  in
  let messages =
    List.fold_left
      (fun map validator -> Map.add validator messages map)
      Chain_messages.empty_messages validators
  in
  let message_amount = message_count messages in
  let messages =
    Chain_filters._filter_messages [ Chain_filters._universal_filter ] messages
  in
  Alcotest.(check' (pair int int))
    ~msg:"universal filter knocks out all messages"
    ~expected:(48 * 4, 0)
    ~actual:(message_amount, Chain_messages.message_count messages)

let test_run_normally () =
  let output = run chains_actions_map ([], empty_messages, 0) 20 in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:"chain runs normally and progresses for 20 rounds"
    ~expected:(true, true)
    ~actual:(level > 1, levels_are_equal)

let test_run_10_round_random_filter () =
  let output =
    run chains_actions_map
      ~filters:(fun round ->
        if round >= 11 then [] else [ Chain_filters._generate_filter () ])
      ([], empty_messages, 0) 40
  in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:"chain runs with filters for 10 rounds, and recovers in 40 rounds"
    ~expected:(true, true)
    ~actual:(level > 1, levels_are_equal)

let test_run_20_round_random_filter () =
  let output =
    run
      ~filters:(fun round ->
        if round <= 20 then [ Chain_filters._generate_filter () ] else [])
      chains_actions_map ([], empty_messages, 0) 80
  in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:"chain runs with filters for 20 rounds, and recovers in 80 rounds"
    ~expected:(true, true)
    ~actual:(level > 1, levels_are_equal)

let test_run_30_round_random_filter () =
  let output =
    run
      ~filters:(fun round ->
        if round <= 30 then [ Chain_filters._generate_filter () ] else [])
      chains_actions_map ([], empty_messages, 0) 120
  in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:"chain runs with filters for 30 rounds, and recovers in 120 rounds"
    ~expected:(true, true)
    ~actual:(level > 1, levels_are_equal)

let test_run_100_round_random_filter () =
  let output =
    run
      ~filters:(fun round ->
        if round <= 100 then [ Chain_filters._generate_filter () ] else [])
      chains_actions_map ([], empty_messages, 0) 130
  in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:"chain runs with filters for 100 rounds, and recovers in 130 rounds"
    ~expected:(true, true)
    ~actual:(level > 4, levels_are_equal)

let test_run_10_round_universal_filter () =
  let output =
    run chains_actions_map
      ~filters:(fun round ->
        if round >= 11 then []
        else (
          Format.eprintf "Passing in universal filter!\n%!";
          [ Chain_filters._universal_filter ]))
      ([], empty_messages, 0) 40
  in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:"chain blocks all messages for 10 rounds, and recovers in 40 rounds"
    ~expected:(true, true)
    ~actual:(level > 3, levels_are_equal)

let test_run_100_round_universal_filter () =
  let output =
    run chains_actions_map
      ~filters:(fun round ->
        if round >= 101 then [] else [ Chain_filters._universal_filter ])
      ([], empty_messages, 0) 150
  in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:"chain blocks all messages for 10 rounds, and recovers in 40 rounds"
    ~expected:(true, true)
    ~actual:(level > 3, levels_are_equal)

let test_run_5_round_message_steal () =
  let output =
    run chains_actions_map ([ List.hd validators ], empty_messages, 5) 40
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
    run chains_actions_map ([ List.hd validators ], empty_messages, 10) 80
  in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:
      "chain runs with stolen messsages for 10 rounds and progresses after 80 \
       rounds"
    ~expected:(true, true)
    ~actual:(level > 3, levels_are_equal)

let test_run_15_round_message_steal () =
  let output =
    run chains_actions_map ([ List.hd validators ], empty_messages, 15) 40
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

let test_run_40_round_message_steal_all () =
  let output = run chains_actions_map (validators, empty_messages, 40) 80 in
  let extraction = extract_levels output in
  let levels_are_equal = elements_are_equal extraction in
  let level = List.hd extraction in
  Alcotest.(check' (pair bool bool))
    ~msg:
      "chain steals all messages for 40 rounds and progresses after 80 rounds"
    ~expected:(true, true)
    ~actual:(level > 3, levels_are_equal)

let run () =
  let open Alcotest in
  run "chain tests" ~and_exit:false
    [
      ( "Utils",
        [ test_case "Universal filter works" `Quick test_universal_filter_work ]
      );
      ( "chain run",
        [
          test_case "normally" `Quick test_run_normally;
          test_case "10_round_random_filter" `Quick
            test_run_10_round_random_filter;
          test_case "20_round_random_filter" `Quick
            test_run_20_round_random_filter;
          test_case "30_round_random_filter" `Quick
            test_run_30_round_random_filter;
          test_case "100_round_random_filter" `Quick
            test_run_100_round_random_filter;
          test_case "10_round_universal_filter" `Quick
            test_run_10_round_universal_filter;
          test_case "100_round_universal_filter" `Quick
            test_run_100_round_universal_filter;
          test_case "5_round_message_steal" `Quick
            test_run_5_round_message_steal;
          test_case "10_round_message_steal" `Quick
            test_run_10_round_message_steal;
          test_case "15_round_message_steal" `Quick
            test_run_15_round_message_steal;
          test_case "40_round_message_steal_all" `Quick
            test_run_40_round_message_steal_all;
        ] );
    ]

let () = run ()
(* Things we can do:
   [ ] Handle lar ge block sizes
   [X] Filter random messages
   [ ] Filter random groups of messages (multiple filters)
   [X] DDos a node? (Universal Filter)
   [X] Send a single set of messages out of order
   [ ] Send random messages out of order
*)
