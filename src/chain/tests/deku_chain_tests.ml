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
open Deku_crypto
open Deku_concepts
open Deku_consensus
open Deku_chain
open Deku_gossip

let get_current () = Chain_genesis.get_current ()

module Map = Chain_genesis.Map

type message_kind = Response | Request | Broadcast
type 'a quantifier = Existential of 'a | Universal

type message_filter =
  | Prevent of {
      from : Key_hash.t quantifier;
      to_ : Key_hash.t quantifier;
      kind : message_kind quantifier;
    }

type local_message =
  | Message of {
      from : Key_hash.t;
      to_ : Key_hash.t;
      raw_expected_hash : string;
      raw_content : string;
      kind : message_kind;
      id : Request_id.t;
    }

let map_find_list = function Some stuff -> stuff | None -> []

(* does filter catch message *)
let catch filter message =
  let eou a b =
    match (a, b) with Universal, _ -> true | Existential a, b -> a = b
  in
  let (Prevent { from = ffrom; to_ = fto_; kind = fkind; _ }) = filter in
  let (Message { from = mfrom; to_ = mto_; kind = mkind; _ }) = message in
  eou ffrom mfrom && eou fto_ mto_ && eou fkind mkind

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

let validators = Chain_genesis.validators

let empty_messages =
  List.fold_left
    (fun map validator -> Map.add validator [] map)
    Map.empty validators

let chains_actions_map = Chain_genesis.chains_actions_map

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
        (fun (all_messages, stolen_messages) validator ->
          let messages_to_steal = Map.find validator all_messages in
          let stolen_messages =
            Map.add validator messages_to_steal stolen_messages
          in
          let all_messages = Map.add validator [] all_messages in
          (all_messages, stolen_messages))
        (all_messages, stolen) to_steal
    in
    (messages, stolen)

let jumble_and_mix messages stolen_messages =
  let shuffle messages =
    Stdlib.Random.self_init ();
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
      let messages = Map.find_opt validator messages in
      match messages with
      | None ->
          Format.eprintf "no messages\n%!";
          (chain, actions)
      | Some messages ->
          Format.eprintf "messages : %d\n%!" (List.length messages);
          List.fold_left message_to_action (chain, actions) messages)
    chain_actions_map

let print_levels chains_actions_map round =
  Map.iter
    (fun _ (chain, _) ->
      let (Chain.Chain { consensus; _ }) = chain in
      let (Consensus.Consensus { current_block; _ }) = consensus in
      let (Block { level; _ }) = current_block in
      let level = Level.to_n level |> Deku_stdlib.N.to_z |> Z.to_int in
      Format.eprintf "round %d, block %d\n%!" round level)
    chains_actions_map

(* Generate random filters of arbitrarily bad scale *)
(* 3 message kinds to block with two quantifiers = 4
   four validators to receive with two quantifiers = 5
   four validators to send with two quantifiers = 5
   total is 4 * 5 * 5 = 100 different filters. Some of which are redundant, but who cares. There's one option which completely kills the network for some amount of time.
*)

let validators_quant = Universal :: List.map (fun v -> Existential v) validators

let kind_quant =
  [
    Existential Response; Existential Request; Existential Broadcast; Universal;
  ]

let _generate_filter () =
  let from = Random.int 5 in
  let to_ = Random.int 5 in
  let kind = Random.int 4 in
  let from = List.nth validators_quant from in
  let to_ = List.nth validators_quant to_ in
  let kind = List.nth kind_quant kind in
  Prevent { from; to_; kind }

let _filter =
  let validator2 = List.nth validators 2 in
  Prevent { from = Universal; to_ = Existential validator2; kind = Universal }

let _universal_filter =
  Prevent { from = Universal; to_ = Universal; kind = Universal }

(* TODO: Generalize to handle many filters *)
let _filter_messages filter messages_to_receive =
  Map.map
    (fun messages ->
      List.filter (fun message -> not (catch filter message)) messages)
    messages_to_receive

let rec loop chains_actions_map round
    ((to_steal, stolen_messages, rounds_left) as thief_stuff) =
  (* TODO: Print out all chain levels *)
  print_levels chains_actions_map round;
  Unix.sleep 1;
  let chains_actions_map, messages_to_receive =
    eval_actions chains_actions_map empty_messages
  in
  let filtered_messages = messages_to_receive in

  (* let filtered_messages = filter_messages Fun.id messages_to_receive in *)
  let messages_to_receive, stolen_messages =
    if rounds_left <= 0 then jumble_and_mix filtered_messages stolen_messages
    else steal_messages filtered_messages thief_stuff
  in
  let chains_actions_map =
    convert_messages_to_actions chains_actions_map messages_to_receive
  in
  loop chains_actions_map (round + 1)
    (to_steal, stolen_messages, rounds_left - 1)

let () = loop chains_actions_map 0 ([ List.hd validators ], empty_messages, 0)

(* Things we can do:
   [ ] Handle large block sizes
   [X] Filter random messages
   [ ] Filter random groups of messages (multiple filters)
   [ ] DDos a node? (Universal Filter)
   [ ] Send a single set of messages out of order
   [ ] Send random messages out of order
*)
