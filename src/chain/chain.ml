open Deku_concepts
open Deku_protocol
open Deku_consensus
open Deku_gossip

type chain =
  | Chain of {
      gossip : Gossip.t;
      protocol : Protocol.t;
      consensus : Consensus.t;
      producer : Producer.t;
      applied : Block.t Block_hash.Map.t;
    }

type t = chain
type fragment = Gossip.fragment [@@deriving show]
type outcome = Gossip.outcome

type action =
  | Chain_trigger_timeout
  | Chain_broadcast of { raw_expected_hash : string; raw_content : string }
  | Chain_save_block of Block.t
  | Chain_send_request of { raw_expected_hash : string; raw_content : string }
  | Chain_send_response of {
      id : Request_id.t; [@opaque]
      raw_expected_hash : string;
      raw_content : string;
    }
  | Chain_send_not_found of { id : Request_id.t [@opaque] }
  | Chain_fragment of { fragment : fragment }
[@@deriving show]

let make ~identity ~validators ~default_block_size =
  let gossip = Gossip.empty in
  let validators = Validators.of_key_hash_list validators in
  let protocol = Protocol.initial in
  let consensus = Consensus.make ~identity ~validators in
  let producer = Producer.make ~identity ~default_block_size in
  let applied = Block_hash.Map.empty in
  Chain { gossip; protocol; consensus; producer; applied }

(* after gossip *)
let rec apply_consensus_action chain consensus_action =
  Logs.debug (fun m ->
      m "Chain: applying consensus action: %a" Consensus.pp_action
        consensus_action);
  let open Consensus in
  match consensus_action with
  | Consensus_accepted_block { block } ->
      let (Chain ({ protocol; producer; applied; _ } as chain)) = chain in
      let (Block { hash; level; payload; tezos_operations; _ }) = block in
      let payload =
        Protocol.prepare ~parallel:(fun f l -> List.filter_map f l) ~payload
      in
      let protocol, receipts =
        Protocol.apply ~current_level:level ~payload ~tezos_operations protocol
      in
      let producer = Producer.clean ~receipts ~tezos_operations producer in
      let applied = Block_hash.Map.add hash block applied in
      let chain = Chain { chain with protocol; producer; applied } in
      (chain, Some (Chain_save_block block))
  | Consensus_trigger_timeout { level } -> (
      let (Chain { consensus; _ }) = chain in
      let (Consensus { current_block; _ }) = consensus in
      let (Block { level = current_level; _ }) = current_block in
      match Level.equal current_level level with
      | true ->
          let action = Chain_trigger_timeout in
          (chain, Some action)
      | false -> (chain, None))
  | Consensus_broadcast_vote { vote } ->
      let content = Message.Content.vote vote in
      let fragment = Gossip.broadcast_message ~content in
      let fragment = Chain_fragment { fragment } in
      (chain, Some fragment)
  | Consensus_request_block { hash } ->
      let content = Request.Content.block hash in
      let fragment = Gossip.send_request ~content in
      let fragment = Chain_fragment { fragment } in
      (chain, Some fragment)

and apply_consensus_actions chain consensus_actions =
  List.fold_left
    (fun (chain, actions) consensus_action ->
      let chain, action = apply_consensus_action chain consensus_action in
      let actions =
        match action with Some action -> action :: actions | None -> actions
      in
      (chain, actions))
    (chain, []) consensus_actions

(* core *)
let incoming_block ~current ~block chain =
  let (Chain ({ consensus; _ } as chain)) = chain in
  let consensus, actions = Consensus.incoming_block ~current ~block consensus in
  let chain = Chain { chain with consensus } in
  apply_consensus_actions chain actions

let incoming_vote ~current ~vote chain =
  let (Chain ({ consensus; _ } as chain)) = chain in
  let consensus, actions = Consensus.incoming_vote ~current ~vote consensus in
  let chain = Chain { chain with consensus } in
  apply_consensus_actions chain actions

let incoming_operation ~operation chain =
  let (Chain ({ producer; _ } as chain)) = chain in
  let producer = Producer.incoming_operation ~operation producer in
  let chain = Chain { chain with producer } in
  (chain, [])

let incoming_bootstrap_signal ~current chain =
  let (Chain { gossip; protocol; consensus; producer; applied }) = chain in
  let consensus =
    match Consensus.incoming_bootstrap_signal ~current consensus with
    | Some consensus -> consensus
    | None -> consensus
  in
  (Chain { gossip; protocol; consensus; producer; applied }, [])

let incoming_tezos_operation ~tezos_operation chain =
  let (Chain { gossip; protocol; consensus; producer; applied }) = chain in
  let producer = Producer.incoming_tezos_operation ~tezos_operation producer in
  (Chain { gossip; protocol; consensus; producer; applied }, [])

let incoming_message ~current ~message chain =
  let open Message in
  let (Message { hash = _; content }) = message in
  match content with
  | Content_block block -> incoming_block ~current ~block chain
  | Content_vote vote -> incoming_vote ~current ~vote chain
  | Content_operation operation -> incoming_operation ~operation chain
  | Content_bootstrap_signal _bootstrap_signal ->
      incoming_bootstrap_signal ~current chain

let incoming_request ~id ~request chain =
  let open Request in
  let (Chain { applied; _ }) = chain in
  let (Request { hash = _; content }) = request in
  match content with
  | Content_block hash -> (
      match Block_hash.Map.find_opt hash applied with
      | Some block ->
          let content = Response.Content.block block in
          let fragment = Gossip.send_response ~id ~content in
          let fragment = Chain_fragment { fragment } in
          (chain, [ fragment ])
      | None ->
          let action = Chain_send_not_found { id } in
          (chain, [ action ]))

let incoming_response ~current ~response chain =
  let open Response in
  let (Response { hash = _; content }) = response in
  match content with
  | Content_block block -> incoming_block ~current ~block chain

let apply_gossip_action ~current ~gossip_action chain =
  match gossip_action with
  | Gossip.Gossip_apply_and_broadcast { message; raw_message } ->
      let chain, actions = incoming_message ~current ~message chain in
      let broadcast =
        let (Raw_message { hash; raw_content }) = raw_message in
        let raw_expected_hash = Message_hash.to_b58 hash in
        Chain_broadcast { raw_expected_hash; raw_content }
      in
      let actions = broadcast :: actions in
      (chain, actions)
  | Gossip.Gossip_send_request { raw_request } ->
      let (Raw_request { hash; raw_content }) = raw_request in
      let raw_expected_hash = Request_hash.to_b58 hash in
      let send = Chain_send_request { raw_expected_hash; raw_content } in
      (chain, [ send ])
  | Gossip.Gossip_incoming_request { id; request } ->
      incoming_request ~id ~request chain
  | Gossip.Gossip_send_response { id; raw_response } ->
      let (Raw_response { hash; raw_content }) = raw_response in
      let raw_expected_hash = Response_hash.to_b58 hash in
      let send = Chain_send_response { id; raw_expected_hash; raw_content } in
      (chain, [ send ])
  | Gossip.Gossip_incoming_response { response } ->
      incoming_response ~current ~response chain
  | Gossip.Gossip_fragment { fragment } ->
      let fragment = Chain_fragment { fragment } in
      (chain, [ fragment ])

(* external *)
let incoming ~raw_expected_hash ~raw_content chain =
  let (Chain ({ gossip; _ } as chain)) = chain in
  let gossip, fragment =
    Gossip.incoming_message ~raw_expected_hash ~raw_content gossip
  in
  let chain = Chain { chain with gossip } in
  (chain, fragment)

let request ~id ~raw_expected_hash ~raw_content chain =
  let fragment = Gossip.incoming_request ~id ~raw_expected_hash ~raw_content in
  (chain, fragment)

let response ~raw_expected_hash ~raw_content chain =
  let fragment = Gossip.incoming_response ~raw_expected_hash ~raw_content in
  (chain, fragment)

let timeout ~current chain =
  let (Chain { consensus; producer; _ }) = chain in
  let fragment =
    match
      Producer.produce
        ~parallel_map:(fun f l -> List.map f l)
        ~current ~consensus producer
    with
    | Some block ->
        let content = Message.Content.block block in
        let fragment = Gossip.broadcast_message ~content in
        Some fragment
    | None -> None
  in
  fragment

let apply ~current ~outcome chain =
  let (Chain ({ gossip; _ } as chain)) = chain in
  let gossip, gossip_action = Gossip.apply ~outcome gossip in
  let chain = Chain { chain with gossip } in
  match gossip_action with
  | Some gossip_action -> apply_gossip_action ~current ~gossip_action chain
  | None -> (chain, [])

let compute fragment = Gossip.compute fragment

let test () =
  let get_current () = Timestamp.of_float (Unix.gettimeofday ()) in

  let open Deku_crypto in
  let secret = Ed25519.Secret.generate () in
  let secret = Secret.Ed25519 secret in
  let identity = Identity.make secret in
  let validators =
    let key = Key.of_secret secret in
    let key_hash = Key_hash.of_key key in
    [ key_hash ]
  in

  let chain = make ~identity ~validators ~default_block_size:0 in
  let (Chain { consensus; _ }) = chain in
  let block =
    let (Consensus { current_block; _ }) = consensus in
    let (Block { hash = current_block; level = current_level; _ }) =
      current_block
    in
    let level = Level.next current_level in
    let previous = current_block in
    let operations = [] in
    let tezos_operations = [] in
    let block =
      Block.produce ~parallel_map:List.map ~identity ~level ~previous
        ~operations ~tezos_operations
    in
    block
  in

  let chain, actions = incoming_block ~current:(get_current ()) ~block chain in
  assert (actions = []);

  let vote = Block.sign ~identity block in
  let chain, actions = incoming_vote ~current:(get_current ()) ~vote chain in
  assert (actions <> []);

  let rec loop chain actions =
    let current = get_current () in
    let chain, actions =
      List.fold_left
        (fun (chain, actions) action ->
          let chain, additional_actions =
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
                (chain, actions)
            | Chain_broadcast { raw_expected_hash; raw_content } ->
                let chain, fragment =
                  incoming ~raw_expected_hash ~raw_content chain
                in
                let actions =
                  match fragment with
                  | Some fragment ->
                      let fragment = Chain_fragment { fragment } in
                      [ fragment ]
                  | None -> []
                in
                (chain, actions)
            | Chain_send_request { raw_expected_hash; raw_content } ->
                let id = Request_id.initial in
                let chain, fragment =
                  request ~id ~raw_expected_hash ~raw_content chain
                in
                let actions =
                  match fragment with
                  | Some fragment ->
                      let fragment = Chain_fragment { fragment } in
                      [ fragment ]
                  | None -> []
                in
                (chain, actions)
            | Chain_send_response { id = _; raw_expected_hash; raw_content } ->
                let chain, fragment =
                  response ~raw_expected_hash ~raw_content chain
                in
                let actions =
                  match fragment with
                  | Some fragment ->
                      let fragment = Chain_fragment { fragment } in
                      [ fragment ]
                  | None -> []
                in
                (chain, actions)
            | Chain_send_not_found { id = _ } -> (chain, [])
            | Chain_fragment { fragment } ->
                let outcome = compute fragment in
                apply ~current ~outcome chain
            | Chain_save_block _ -> (chain, [])
          in
          (chain, actions @ additional_actions))
        (chain, []) actions
    in
    loop chain actions
  in
  loop chain actions
