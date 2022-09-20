(* You have the same powers as an adversary in the partially synchronous model. You can split the network however you like.
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

let get_current () = Timestamp.of_float (Unix.gettimeofday ())
let secret1 = Ed25519.Secret.generate ()
let secret1 = Secret.Ed25519 secret1
let identity1 = Identity.make secret1
let secret2 = Ed25519.Secret.generate ()
let secret2 = Secret.Ed25519 secret2
let identity2 = Identity.make secret2
let secret3 = Ed25519.Secret.generate ()
let secret3 = Secret.Ed25519 secret3
let identity3 = Identity.make secret3
let secret4 = Ed25519.Secret.generate ()
let secret4 = Secret.Ed25519 secret4
let identity4 = Identity.make secret4

let validators =
  let key1 = Key.of_secret secret1 in
  let key2 = Key.of_secret secret2 in
  let key3 = Key.of_secret secret3 in
  let key4 = Key.of_secret secret4 in
  let key_hash1 = Key_hash.of_key key1 in
  let key_hash2 = Key_hash.of_key key2 in
  let key_hash3 = Key_hash.of_key key3 in
  let key_hash4 = Key_hash.of_key key4 in
  [ key_hash1; key_hash2; key_hash3; key_hash4 ]

(* We have the sender, receiver, and backup chains.
   Now we create some blocks, change the chain states,
   and mess with the perception of the receiver chain
*)
let sender_chain = Chain.make ~identity:identity1 ~validators
let receiver_chain = Chain.make ~identity:identity2 ~validators
let backup_chain1 = Chain.make ~identity:identity3 ~validators
let backup_chain2 = Chain.make ~identity:identity4 ~validators
let chains = [ sender_chain; receiver_chain; backup_chain1; backup_chain2 ]

let first_block =
  let leader = List.nth chains 0 in
  let (Chain.Chain { consensus; _ }) = leader in
  let (Consensus { current_block; identity; _ }) = consensus in
  let (Block { hash = current_block; level = current_level; _ }) =
    current_block
  in
  let level = Level.next current_level in
  let previous = current_block in
  let operations = [] in
  Block.produce ~identity ~level ~previous ~operations

(* Chains after we've applied votes from the first block
   Does not drive code, for initialization only *)
let chains_after_first_block =
  (* Chains and actions after incoming blocks *)
  let chains_actions =
    List.map
      (fun chain ->
        Chain.incoming_block ~current:(get_current ()) ~block:first_block chain)
      chains
  in

  (* Collect votes to first block from all chains *)
  let votes =
    List.map
      (fun chain ->
        let (Chain.Chain { consensus; _ }) = chain in
        let (Consensus { identity; _ }) = consensus in
        Block.sign ~identity first_block)
      chains
  in

  (* Apply votes to chain, actions *)
  let apply_votes chain =
    List.fold_left
      (fun (chain, initial_actions) vote ->
        let chain, actions =
          Chain.incoming_vote ~current:(get_current ()) ~vote chain
        in
        (chain, initial_actions @ actions))
      chain votes
  in

  List.map apply_votes chains_actions

module Map = Key_hash.Map

let map_find_log msg = function Some stuff -> stuff | None -> failwith msg

let chain_actions_map =
  let module Map = Key_hash.Map in
  List.fold_left2
    (fun map validator chain_actions -> Map.add validator chain_actions map)
    Map.empty validators chains_after_first_block

(* Process handlers in chainl.ml:

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
*)

type message_kind = Response | Request | Broadcast

(* type message_filter =
   | Prevent of {
       from : Key_hash.t;
       to_ : Key_hash.t;
       raw_expected_hash : string;
       raw_content : string;
       kind : message_kind;
     } *)

type local_message =
  | Message of {
      from : Key_hash.t;
      to_ : Key_hash.t;
      raw_expected_hash : string;
      raw_content : string;
      kind : message_kind;
      id : Request_id.t;
    }

(* does filter catch message *)
(* let catch filter message =
   let (Prevent { from = ffrom; to_ = fto_; kind = fkind; _ }) = filter in
   let (Message { from = mfrom; to_ = mto_; kind = mkind; _ }) = message in
   ffrom = mfrom && fto_ = mto_ && fkind = mkind *)

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
(* TODO: How many times do we need to run this to make progress?
   Who cares? We only need to add conditionals for the weird cases later *)
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
    match action with
    | Chain_trigger_timeout ->
        let fragment = timeout ~current chain in
        let actions =
          match fragment with
          | Some fragment ->
              let fragment = Chain_fragment { fragment } in
              [ fragment ]
          | None -> []
        in
        (chain, actions, messages_to_receive)
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
        (chain, actions, new_messages)
    | Chain_send_request { raw_expected_hash; raw_content } ->
        let to_ =
          let rec get_someone_else () =
            let validator =
              List.nth validator_list (Random.int validators_len - 1)
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
            (message
            :: (Map.find_opt to_ messages_to_receive
               |> map_find_log "fails at 258"))
            messages_to_receive
        in
        (chain, actions, new_messages)
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
            (message
            :: (Map.find_opt to_ messages_to_receive
               |> map_find_log "fails at 277"))
            messages_to_receive
        in
        (chain, actions, new_messages)
    | Chain_send_not_found { id = _ } -> (chain, actions, messages_to_receive)
    | Chain_fragment { fragment } ->
        let outcome = compute fragment in
        let chain, actions = apply ~current ~outcome chain in
        (chain, actions, messages_to_receive)
  in
  (chain, additional_actions @ actions, messages_to_receive)

(* Breaks up evaluation into execution and broadcasting.
   Broadcasting is always performed after execution.
   This makes things conceptually simpler and also prevents potential bugs. *)
let eval chain_actions_map =
  (* Iterate through all the present actions *)
  let chain_actions_maps, messages_to_receive =
    Map.fold
      (fun validator (chain, actions) (new_map, messages_to_receive) ->
        let chain, new_actions, messages_to_receive =
          List.fold_left process_chain_action
            (chain, [], messages_to_receive)
            actions
        in
        (Map.add validator (chain, new_actions) new_map, messages_to_receive))
      chain_actions_map
      (chain_actions_map, Map.empty)
  in

  (* TODO: Add message filter *)
  (* let filters =
     let chains_action_map = *)

  (* Convert sent messages to chain actions *)
  let chains_action_map =
    Map.mapi
      (fun validator (chain, (actions : Chain.action list)) ->
        let messages = Map.find_opt validator messages_to_receive in
        (* let messages = map_find_log "fails at 317" messages in *)
        match messages with
        | None -> (chain, actions)
        | Some messages ->
            List.fold_left message_to_action (chain, actions) messages)
      chain_actions_maps
  in
  chains_action_map

(* TODO: Case where we prevent any initial messages to someone from getting through. Make sure that those messages will eventually be received by the person *)

(* let second_state = eval chain_actions_map
   let third_state = eval second_state *)
let () =
  let rec loop chain_actions_map =
    Format.eprintf "called loop\n%!";
    Unix.sleep 1;
    let chain_actions_map = eval chain_actions_map in
    loop chain_actions_map
  in
  loop chain_actions_map
