open Deku_stdlib
open Deku_concepts
open Deku_protocol
open Deku_consensus
open Deku_gossip

type chain =
  | Chain of {
      pool : Parallel.Pool.t;
      protocol : Protocol.t;
      consensus : Consensus.t;
      producer : Producer.t;
    }

type t = chain

type action =
  | Chain_trigger_timeout
  | Chain_broadcast of { content : Message.Content.t }

let make ~identity ~validators ~pool =
  let validators = Validators.of_key_hash_list validators in
  let protocol = Protocol.initial in
  let consensus = Consensus.make ~identity ~validators in
  let producer = Producer.make ~identity in
  Chain { pool; protocol; consensus; producer }

let rec apply_consensus_action chain consensus_action =
  let open Consensus in
  let (Chain { pool; protocol; consensus; producer }) = chain in
  match consensus_action with
  | Consensus_accepted_block { level; payload } ->
      let protocol, receipts =
        Protocol.apply
          ~parallel:(fun f l -> Parallel.filter_map_p pool f l)
          ~current_level:level ~payload protocol
      in
      let producer = Producer.clean ~receipts producer in
      let chain = Chain { pool; protocol; consensus; producer } in
      (chain, None)
  | Consensus_trigger_timeout { level } -> (
      let (Consensus { state; _ }) = consensus in
      let (State.State { current_level; _ }) = state in
      match Level.equal current_level level with
      | true ->
          let action = Chain_trigger_timeout in
          (chain, Some action)
      | false -> (chain, None))
  | Consensus_broadcast_signature { signature } ->
      let content = Message.Content.signature signature in
      let action = Chain_broadcast { content } in
      (chain, Some action)

and apply_consensus_actions chain consensus_actions =
  List.fold_left
    (fun (chain, actions) consensus_action ->
      let chain, action = apply_consensus_action chain consensus_action in
      let actions =
        match action with Some action -> action :: actions | None -> actions
      in
      (chain, actions))
    (chain, []) consensus_actions

and incoming_block ~current ~block chain =
  let (Chain { pool; protocol; consensus; producer }) = chain in
  let consensus, effects = Consensus.incoming_block ~current ~block consensus in
  let chain = Chain { pool; protocol; consensus; producer } in
  apply_consensus_actions chain effects

let incoming_signature ~current ~signature chain =
  let (Chain { pool; protocol; consensus; producer }) = chain in
  let consensus, effects =
    Consensus.incoming_signature ~current ~signature consensus
  in
  let chain = Chain { pool; protocol; consensus; producer } in
  apply_consensus_actions chain effects

let incoming_operation ~operation node =
  let (Chain { pool; protocol; consensus; producer }) = node in
  let producer = Producer.incoming_operation ~operation producer in
  Chain { pool; protocol; consensus; producer }

let incoming_message ~current ~content chain =
  let open Message.Content in
  match content with
  | Content_block block -> incoming_block ~current ~block chain
  | Content_signature signature -> incoming_signature ~current ~signature chain
  | Content_operation operation ->
      let chain = incoming_operation ~operation chain in
      (chain, [])

let incoming_timeout ~current chain =
  let (Chain { pool; protocol; consensus; producer }) = chain in
  let chain = Chain { pool; protocol; consensus; producer } in
  let actions =
    let (Consensus { block_pool = _; signer = _; state }) = consensus in
    match Producer.produce ~current ~state producer with
    | Some block ->
        let content = Message.Content.block block in
        [ Chain_broadcast { content } ]
    | None -> []
  in
  (chain, actions)

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
    let (Consensus.Consensus { state; _ }) = consensus in
    let (State { current_level; current_block; _ }) = state in
    let level = Level.next current_level in
    let previous = current_block in
    let operations = [] in
    let block = Block.produce ~identity ~level ~previous ~operations in
    block
  in

  let chain, actions = incoming_block ~current:(get_current ()) ~block chain in
  assert (actions = []);

  let signature = Block.sign ~identity block in
  let chain, actions =
    incoming_signature ~current:(get_current ()) ~signature chain
  in
  assert (actions <> []);

  let rec loop chain actions =
    let current = get_current () in
    let chain, actions =
      List.fold_left
        (fun (chain, actions) action ->
          let chain, additional_actions =
            match action with
            | Chain_trigger_timeout -> incoming_timeout ~current chain
            | Chain_broadcast { content } ->
                incoming_message ~current ~content chain
          in
          (chain, actions @ additional_actions))
        (chain, []) actions
    in
    loop chain actions
  in
  loop chain actions
