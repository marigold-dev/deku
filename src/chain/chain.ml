open Deku_stdlib
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

let apply_consensus_action chain consensus_action =
  let open Consensus in
  match consensus_action with
  | Consensus_accepted_block { level; payload } ->
      let (Chain { pool; protocol; consensus; producer }) = chain in
      let protocol, receipts =
        Protocol.apply
          ~parallel:(fun f l -> Parallel.filter_map_p pool f l)
          ~current_level:level ~payload protocol
      in
      let producer = Producer.clean ~receipts producer in
      let chain = Chain { pool; protocol; consensus; producer } in
      (chain, None)
  | Consensus_trigger_timeout -> (chain, Some Chain_trigger_timeout)
  | Consensus_broadcast_signature { signature } ->
      let content = Message.Content.signature signature in
      let action = Chain_broadcast { content } in
      (chain, Some action)

let apply_consensus_actions chain consensus_actions =
  List.fold_left
    (fun (chain, actions) consensus_action ->
      let chain, action = apply_consensus_action chain consensus_action in
      let actions =
        match action with Some action -> action :: actions | None -> actions
      in
      (chain, actions))
    (chain, []) consensus_actions

let incoming_block ~current ~block chain =
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

let incoming_message ~current ~message chain =
  let open Message in
  let (Message { hash = _; content }) = message in
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
