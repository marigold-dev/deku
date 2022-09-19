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
  | Chain_save_block of Block.t

let make ~identity ~bootstrap_key ~validators ~pool ~default_block_size =
  let validators = Validators.of_key_hash_list validators in
  let protocol = Protocol.initial in
  let consensus = Consensus.make ~identity ~validators ~bootstrap_key in
  let producer = Producer.make ~identity ~default_block_size in
  Chain { pool; protocol; consensus; producer }

let rec apply_consensus_action chain consensus_action =
  let open Consensus in
  let (Chain { pool; protocol; consensus; producer }) = chain in
  match consensus_action with
  | Consensus_accepted_block block ->
      let (Block.Block { level; payload; tezos_operations; _ }) = block in
      let protocol, receipts =
        Protocol.apply
          ~parallel:(fun f l -> Parallel.filter_map_p pool f l)
          ~current_level:level ~payload protocol ~tezos_operations
      in
      let producer = Producer.clean ~receipts ~tezos_operations producer in
      let chain = Chain { pool; protocol; consensus; producer } in
      (chain, Some (Chain_save_block block))
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

let incoming_bootstrap_signal ~bootstrap_signal ~current chain =
  let (Chain { pool; protocol; consensus; producer }) = chain in
  let consensus =
    match
      Consensus.incoming_bootstrap_signal ~bootstrap_signal ~current consensus
    with
    | Some consensus -> consensus
    | None -> consensus
  in
  let effects =
    let (Consensus { block_pool = _; signer = _; bootstrap_key = _; state }) =
      consensus
    in
    match
      Producer.produce ~current ~state
        ~parallel_map:(fun f l -> Parallel.map_p pool f l)
        producer
    with
    | Some block ->
        let content = Message.Content.block block in
        [ Chain_broadcast { content } ]
    | None -> []
  in
  (Chain { pool; protocol; consensus; producer }, effects)

let incoming_tezos_operation ~tezos_operation chain =
  let (Chain { pool; protocol; consensus; producer }) = chain in
  let producer = Producer.incoming_tezos_operation ~tezos_operation producer in
  (Chain { pool; protocol; consensus; producer }, [])

let incoming_message ~current ~message chain =
  let open Message in
  let (Message { hash = _; content }) = message in
  match content with
  | Content_block block -> incoming_block ~current ~block chain
  | Content_signature signature -> incoming_signature ~current ~signature chain
  | Content_operation operation ->
      let chain = incoming_operation ~operation chain in
      (chain, [])
  | Content_bootstrap_signal bootstrap_signal ->
      incoming_bootstrap_signal ~bootstrap_signal ~current chain

let incoming_timeout ~current chain =
  let (Chain { pool; protocol; consensus; producer }) = chain in
  let chain = Chain { pool; protocol; consensus; producer } in
  let actions =
    let (Consensus { block_pool = _; signer = _; state; bootstrap_key = _ }) =
      consensus
    in
    (* FIXME: I don't like having to duplicate the parallel_map thing *)
    match
      Producer.produce
        ~parallel_map:(fun f l -> Parallel.map_p pool f l)
        ~current ~state producer
    with
    | Some block ->
        let content = Message.Content.block block in
        [ Chain_broadcast { content } ]
    | None -> []
  in
  (chain, actions)
