open Deku_stdlib
open Deku_crypto
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
      applied : Block.t Block_hash.Map.t;
    }

type t = chain

type action =
  | Chain_trigger_timeout
  | Chain_broadcast of { content : Message.Content.t }
  | Chain_save_block of Block.t
  | Chain_send of { to_ : Key_hash.t; content : Message.Content.t }
[@@deriving show]

let make ~identity ~validators ~pool ~default_block_size =
  let validators = Validators.of_key_hash_list validators in
  let protocol = Protocol.initial in
  let consensus = Consensus.make ~identity ~validators in
  let producer = Producer.make ~identity ~default_block_size in
  let applied = Block_hash.Map.empty in
  Chain { pool; protocol; consensus; producer; applied }

let rec apply_consensus_action chain consensus_action =
  Logs.debug (fun m ->
      m "Chain: applying consensus action: %a" Consensus.pp_action
        consensus_action);
  let open Consensus in
  let (Chain { pool; protocol; consensus; producer; applied }) = chain in
  match consensus_action with
  | Consensus_accepted_block { block } ->
      let (Block { hash; level; payload; tezos_operations; _ }) = block in
      let protocol, receipts =
        Protocol.apply
          ~parallel:(fun f l -> Parallel.filter_map_p pool f l)
          ~current_level:level ~payload protocol ~tezos_operations
      in
      let producer = Producer.clean ~receipts ~tezos_operations producer in
      let applied = Block_hash.Map.add hash block applied in
      let chain = Chain { pool; protocol; consensus; producer; applied } in
      (chain, Some (Chain_save_block block))
  | Consensus_trigger_timeout { level } -> (
      let (Consensus { current_block; _ }) = consensus in
      let (Block { level = current_level; _ }) = current_block in
      match Level.equal current_level level with
      | true ->
          let action = Chain_trigger_timeout in
          (chain, Some action)
      | false -> (chain, None))
  | Consensus_broadcast_vote { vote } ->
      let content = Message.Content.vote vote in
      let action = Chain_broadcast { content } in
      (chain, Some action)
  | Consensus_request_block { self; hash } ->
      let content = Message.Content.request_block ~to_:self ~hash in
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
  let (Chain { pool; protocol; consensus; producer; applied }) = chain in
  let consensus, effects = Consensus.incoming_block ~current ~block consensus in
  let chain = Chain { pool; protocol; consensus; producer; applied } in
  apply_consensus_actions chain effects

let incoming_vote ~current ~vote chain =
  let (Chain { pool; protocol; consensus; producer; applied }) = chain in
  let consensus, actions = Consensus.incoming_vote ~current ~vote consensus in
  let chain = Chain { pool; protocol; consensus; producer; applied } in
  apply_consensus_actions chain actions

let incoming_operation ~operation chain =
  let (Chain { pool; protocol; consensus; producer; applied }) = chain in
  let producer = Producer.incoming_operation ~operation producer in
  let chain = Chain { pool; protocol; consensus; producer; applied } in
  (chain, [])

let incoming_request_block ~to_ ~hash chain =
  let (Chain { applied; _ }) = chain in
  match Block_hash.Map.find_opt hash applied with
  | Some block ->
      (* TODO: this is very inneficient as it serializes the block many times *)
      let content = Message.Content.block block in
      (* TODO: not broadcast *)
      (chain, [ Chain_send { to_; content } ])
  | None -> (chain, [])

let incoming_bootstrap_signal ~current chain =
  let (Chain { pool; protocol; consensus; producer; applied }) = chain in
  let consensus =
    match Consensus.incoming_bootstrap_signal ~current consensus with
    | Some consensus -> consensus
    | None -> consensus
  in
  let effects =
    match
      Producer.produce ~current ~consensus
        ~parallel_map:(fun f l -> Parallel.map_p pool f l)
        producer
    with
    | Some block ->
        Logs.debug (fun m -> m "Broadcasting block");
        let content = Message.Content.block block in
        [ Chain_broadcast { content } ]
    | None -> []
  in
  (Chain { pool; protocol; consensus; producer; applied }, effects)

let incoming_tezos_operation ~tezos_operation chain =
  let (Chain { pool; protocol; consensus; producer; applied }) = chain in
  let producer = Producer.incoming_tezos_operation ~tezos_operation producer in
  (Chain { pool; protocol; consensus; producer; applied }, [])

let incoming_message ~current ~content chain =
  Logs.debug (fun m -> m "Incoming message: %a" Message.Content.pp content);
  let open Message.Content in
  match content with
  | Content_block block -> incoming_block ~current ~block chain
  | Content_vote vote -> incoming_vote ~current ~vote chain
  | Content_operation operation -> incoming_operation ~operation chain
  | Content_request_block { to_; hash } ->
      incoming_request_block ~to_ ~hash chain
  | Content_bootstrap_signal _bootstrap_signal ->
      incoming_bootstrap_signal ~current chain

let incoming_timeout ~current chain =
  let (Chain { pool; protocol; consensus; producer; applied }) = chain in
  let chain = Chain { pool; protocol; consensus; producer; applied } in
  let actions =
    match
      Producer.produce ~current ~consensus
        ~parallel_map:(fun f l -> Parallel.map_p pool f l)
        producer
    with
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

  let chain = make ~identity ~validators ~pool ~default_block_size:0 in
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
            | Chain_trigger_timeout -> incoming_timeout ~current chain
            | Chain_broadcast { content } ->
                incoming_message ~current ~content chain
            | Chain_save_block _ -> (chain, [])
            | Chain_send { to_ = _; content } ->
                incoming_message ~current ~content chain
          in
          (chain, actions @ additional_actions))
        (chain, []) actions
    in
    loop chain actions
  in
  loop chain actions
