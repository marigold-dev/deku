open Deku_crypto
open Deku_concepts
open Deku_consensus
open Deku_chain
open Deku_gossip
open Chain_genesis
open Chain_messages

let map_find_list = function Some stuff -> stuff | None -> []
let request = Atomic.make Request_id.initial

let get_incr () =
  let req = Atomic.get request in
  Atomic.set request (Request_id.next req);
  req

let id_handler = Hashtbl.create 32

let message_to_action (chain, actions) message =
  let (Message { from; raw_expected_hash; raw_content; kind; id = _id; _ }) =
    message
  in
  let chain, fragment =
    match kind with
    | Response -> Chain.response ~raw_expected_hash ~raw_content chain
    | Request ->
        let id = get_incr () in
        Hashtbl.add id_handler id from;
        Chain.request ~id ~raw_expected_hash ~raw_content chain
    | Broadcast -> Chain.incoming ~raw_expected_hash ~raw_content chain
  in
  let actions =
    match fragment with
    | Some fragment ->
        let fragment = Chain.Chain_fragment { fragment } in
        fragment :: actions
    | None -> actions
  in
  (chain, actions)

(* Handles a single iteration of chain actions and saves fragments as actions. Also saves broadcasts for later consumption. *)
let process_chain_action (chain, actions, messages_to_receive) action =
  let module Map = Key_hash.Map in
  let open Chain in
  let (Chain { consensus; _ }) = chain in
  let (Consensus { identity; validators; _ }) = consensus in
  let validator_list = Validators.to_key_hash_list validators in
  let validators_len = List.length validator_list in
  let from = Identity.key_hash identity in
  let current = get_current () in
  let chain, additional_actions, messages_to_receive =
    let new_actions = [] in
    match action with
    | Chain_trigger_timeout ->
        let fragment = timeout ~current chain in
        let new_actions =
          match fragment with
          | Some fragment ->
              let fragment = Chain_fragment { fragment } in
              fragment :: new_actions
          | None -> []
        in
        (chain, new_actions, messages_to_receive)
    | Chain_broadcast { raw_expected_hash; raw_content } ->
        let new_messages =
          Map.mapi
            (fun validator messages ->
              let message =
                Message
                  {
                    from;
                    to_ = validator;
                    raw_expected_hash;
                    raw_content;
                    kind = Broadcast;
                    id = Request_id.initial;
                  }
              in

              message :: messages)
            messages_to_receive
        in
        (chain, new_actions, new_messages)
    | Chain_send_request { raw_expected_hash; raw_content } ->
        let to_ =
          let rec get_someone_else () =
            let validator =
              let validator =
                List.nth validator_list (Random.int (validators_len - 1))
              in
              validator
            in
            if validator = from then get_someone_else () else validator
          in
          get_someone_else ()
        in

        let message =
          Message
            {
              from;
              to_;
              raw_expected_hash;
              raw_content;
              kind = Request;
              id = Request_id.initial;
            }
        in
        let new_messages =
          Map.add to_
            (message :: (Map.find_opt to_ messages_to_receive |> map_find_list))
            messages_to_receive
        in
        (chain, new_actions, new_messages)
    | Chain_send_response { id; raw_expected_hash; raw_content } ->
        let to_ = Hashtbl.find id_handler id in
        let message =
          Message
            {
              from;
              to_;
              raw_expected_hash;
              raw_content;
              kind = Response;
              id = Request_id.initial;
            }
        in
        let new_messages =
          Map.add to_
            (message :: (Map.find_opt to_ messages_to_receive |> map_find_list))
            messages_to_receive
        in
        (chain, new_actions, new_messages)
    | Chain_send_not_found { id = _ } ->
        (chain, new_actions, messages_to_receive)
    | Chain_fragment { fragment } ->
        let outcome = compute fragment in
        let chain, new_actions = apply ~current ~outcome chain in
        (chain, new_actions, messages_to_receive)
  in
  let actions = additional_actions @ actions in
  (chain, actions, messages_to_receive)

(* Breaks up evaluation into execution and broadcasting.
   Broadcasting is always performed after execution.
   This makes things conceptually simpler and also prevents potential bugs.
   Uses func filters to eliminate messages from the message pool *)

let eval_actions chains_actions_map messages_to_receive =
  Map.fold
    (fun validator (chain, actions) (new_map, messages_to_receive) ->
      let chain, new_actions, messages_to_receive =
        List.fold_left
          (fun acc action ->
            Chain.pp_action action;
            let chain, actions, messages_to_receive =
              process_chain_action acc action
            in
            (chain, actions, messages_to_receive))
          (chain, [], messages_to_receive)
          actions
      in
      (Map.add validator (chain, new_actions) new_map, messages_to_receive))
    chains_actions_map
    (chains_actions_map, messages_to_receive)

let steal_messages all_messages thief_stuff =
  let to_steal, stolen, rounds_left = thief_stuff in
  if rounds_left <= 0 then (all_messages, stolen)
  else
    let messages, stolen =
      List.fold_left
        (fun (all_messages, all_stolen_messages) validator ->
          let messages_to_steal = Map.find validator all_messages in
          let stolen_messages = Map.find validator all_stolen_messages in
          let all_stolen_messages =
            Map.add validator
              (stolen_messages @ messages_to_steal)
              all_stolen_messages
          in
          let all_messages = Map.add validator [] all_messages in
          (all_messages, all_stolen_messages))
        (all_messages, stolen) to_steal
    in
    (messages, stolen)

let jumble_and_mix messages stolen_messages =
  let shuffle messages =
    Stdlib.Random.init 0;
    let cmp = List.map (fun c -> (Stdlib.Random.bits (), c)) messages in
    let sond = List.sort compare cmp in
    List.map snd sond
  in
  let all_messages =
    Map.fold
      (fun validator filtered_messages all_messages ->
        let stolen_messages = shuffle @@ Map.find validator stolen_messages in
        Map.add validator (stolen_messages @ filtered_messages) all_messages)
      messages empty_messages
  in
  (all_messages, empty_messages)

let convert_messages_to_actions chain_actions_map messages =
  Map.mapi
    (fun validator (chain, actions) ->
      let messages = Map.find validator messages in
      List.fold_left message_to_action (chain, actions) messages)
    chain_actions_map

(* Generate random filters of arbitrarily bad scale *)
(* 3 message kinds to block with two quantifiers = 4
   four validators to receive with two quantifiers = 5
   four validators to send with two quantifiers = 5
   total is 4 * 5 * 5 = 100 different filters. Some of which are redundant, but who cares. There's one option which completely kills the network for some amount of time.
*)
(* TODO: How do we control filters over time? *)
(* TODO: Change thief stuff into a closure with a function similar to filters *)

let rec run chains_actions_map ?(round = 0) ?(filters = fun _ -> [])
    ((to_steal, stolen_messages, rounds_left) as thief_stuff) stopping_point =
  match round = stopping_point with
  | true -> chains_actions_map
  | false ->
      Chain_printers.print_round_levels chains_actions_map round;
      let chains_actions_map, messages_to_receive =
        eval_actions chains_actions_map empty_messages
      in
      let filter_list = filters round in
      let filtered_messages =
        let open Chain_filters in
        _filter_messages filter_list messages_to_receive
      in
      let messages_to_receive, stolen_messages =
        if rounds_left < 0 then (filtered_messages, stolen_messages)
        else if rounds_left = 0 then
          jumble_and_mix filtered_messages stolen_messages
        else steal_messages filtered_messages thief_stuff
      in
      Format.eprintf "We've stolen %d messages!\n%!"
        (Chain_messages.message_count stolen_messages);
      Format.eprintf "There are %d messages!\n%!"
        (Chain_messages.message_count messages_to_receive);
      Chain_printers.print_messages messages_to_receive;
      let chains_actions_map =
        convert_messages_to_actions chains_actions_map messages_to_receive
      in
      run chains_actions_map ~round:(round + 1) ~filters
        (to_steal, stolen_messages, rounds_left - 1)
        stopping_point
