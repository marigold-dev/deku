open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_protocol
open Deku_consensus
open Deku_gossip

type chain =
  | Chain of {
      pool : Parallel.Pool.t;
      gossip : Gossip.t;
      protocol : Protocol.t;
      consensus : Consensus.t;
      producer : Producer.t;
      applied : Block.t Block_hash.Map.t;
    }

type t = chain
type fragment = Gossip.fragment
type outcome = Gossip.outcome

type action =
  | Chain_trigger_timeout
  | Chain_broadcast of { raw_expected_hash : string; raw_content : string }
  | Chain_send of {
      to_ : Key_hash.t;
      raw_expected_hash : string;
      raw_content : string;
    }
  | Chain_fragment of { fragment : fragment }

let make ~identity ~validators ~pool =
  let gossip = Gossip.empty in
  let validators = Validators.of_key_hash_list validators in
  let protocol = Protocol.initial in
  let consensus = Consensus.make ~identity ~validators in
  let producer = Producer.make ~identity in
  let applied = Block_hash.Map.empty in
  Chain { pool; gossip; protocol; consensus; producer; applied }

(* after gossip *)
let rec apply_consensus_action chain consensus_action =
  let open Consensus in
  match consensus_action with
  | Consensus_accepted_block { block } ->
      let (Chain ({ pool; protocol; producer; applied; _ } as chain)) = chain in
      let (Block { hash; level; payload; _ }) = block in
      let protocol, receipts =
        Protocol.apply
          ~parallel:(fun f l -> Parallel.filter_map_p pool f l)
          ~current_level:level ~payload protocol
      in
      let producer = Producer.clean ~receipts producer in
      let applied = Block_hash.Map.add hash block applied in
      let chain = Chain { chain with protocol; producer; applied } in
      (chain, None)
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
      let fragment = Gossip.broadcast ~content in
      let fragment = Chain_fragment { fragment } in
      (chain, Some fragment)
  | Consensus_request_block { self; hash } ->
      let content = Message.Content.request_block ~to_:self ~hash in
      let fragment = Gossip.broadcast ~content in
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

let incoming_request_block ~to_ ~hash chain =
  let (Chain { applied; _ }) = chain in
  match Block_hash.Map.find_opt hash applied with
  | Some block ->
      (* TODO: this is very inneficient as it serializes the block many times *)
      let content = Message.Content.block block in
      let fragment = Gossip.send ~to_ ~content in
      let fragment = Chain_fragment { fragment } in
      (chain, [ fragment ])
  | None -> (chain, [])

let incoming_message ~current ~message chain =
  let open Message in
  let (Message { hash = _; content }) = message in
  match content with
  | Content_block block -> incoming_block ~current ~block chain
  | Content_vote vote -> incoming_vote ~current ~vote chain
  | Content_operation operation -> incoming_operation ~operation chain
  | Content_request_block { to_; hash } ->
      incoming_request_block ~to_ ~hash chain

let apply_gossip_action ~current ~gossip_action chain =
  let open Gossip in
  match gossip_action with
  | Gossip_apply_and_broadcast { message; raw_message } ->
      let chain, actions = incoming_message ~current ~message chain in
      let broadcast =
        let (Raw_message { hash; raw_content }) = raw_message in
        let raw_expected_hash = Message_hash.to_b58 hash in
        Chain_broadcast { raw_expected_hash; raw_content }
      in
      let actions = broadcast :: actions in
      (chain, actions)
  | Gossip_send { to_; raw_message } ->
      let (Raw_message { hash; raw_content }) = raw_message in
      let raw_expected_hash = Message_hash.to_b58 hash in
      let send = Chain_send { to_; raw_expected_hash; raw_content } in
      (chain, [ send ])
  | Gossip_fragment { fragment } ->
      let fragment = Chain_fragment { fragment } in
      (chain, [ fragment ])

(* external *)
let incoming ~raw_expected_hash ~raw_content chain =
  let (Chain ({ gossip; _ } as chain)) = chain in
  let gossip, fragment =
    Gossip.incoming ~raw_expected_hash ~raw_content gossip
  in
  let chain = Chain { chain with gossip } in
  (chain, fragment)

let timeout ~current chain =
  let (Chain { consensus; producer; _ }) = chain in
  let fragment =
    match Producer.produce ~current ~consensus producer with
    | Some block ->
        let content = Message.Content.block block in
        let fragment = Gossip.broadcast ~content in
        Some fragment
    | None -> None
  in
  fragment

let apply ~current ~outcome chain =
  let (Chain ({ gossip; _ } as chain)) = chain in
  let gossip, gossip_action =
    Gossip.apply ~current:(Timestamp.to_float current) ~outcome gossip
  in
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
  let pool = Parallel.Pool.make ~domains:8 in

  let chain = make ~identity ~validators ~pool in
  let (Chain { consensus; _ }) = chain in
  let block =
    let (Consensus { current_block; _ }) = consensus in
    let (Block { hash = current_block; level = current_level; _ }) =
      current_block
    in
    let level = Level.next current_level in
    let previous = current_block in
    let operations = [] in
    let block = Block.produce ~identity ~level ~previous ~operations in
    block
  in

  let chain, actions = incoming_block ~current:(get_current ()) ~block chain in
  Format.printf "actions: %d\n%!" (List.length actions);
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
            | Chain_broadcast { raw_expected_hash; raw_content }
            | Chain_send { to_ = _; raw_expected_hash; raw_content } ->
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
            | Chain_fragment { fragment } ->
                let outcome = compute fragment in
                apply ~current ~outcome chain
          in
          (chain, actions @ additional_actions))
        (chain, []) actions
    in
    loop chain actions
  in
  loop chain actions
