open Deku_stdlib
open Deku_concepts
open Deku_protocol
open Deku_consensus

type chain =
  | Chain of {
      pool : Parallel.Pool.t;
      protocol : Protocol.t;
      consensus : Consensus.t;
      producer : Producer.t;
    }

type t = chain

type external_effect =
  (* timer *)
  | Trigger_timeout
  (* network *)
  | Broadcast_block of { block : Block.t }
  | Broadcast_signature of { signature : Verified_signature.t }

let make ~identity ~validators ~pool =
  let validators = Validators.of_key_hash_list validators in
  let protocol = Protocol.initial in
  let consensus = Consensus.make ~identity ~validators in
  let producer = Producer.make ~identity in
  Chain { pool; protocol; consensus; producer }

let handle_consensus_effect chain effects : _ * external_effect option =
  let open Consensus in
  let (Chain { pool; protocol; consensus; producer }) = chain in
  match effects with
  | Accepted_block { level; payload } ->
      let protocol, receipts =
        Protocol.apply
          ~parallel:(fun f l -> Parallel.filter_map_p pool f l)
          ~current_level:level ~payload protocol
      in
      let producer = Producer.clean ~receipts producer in
      let chain = Chain { pool; protocol; consensus; producer } in
      (chain, None)
  | Trigger_timeout -> (chain, Some Trigger_timeout)
  | Broadcast_signature { signature } ->
      (chain, Some (Broadcast_signature { signature }))

let handle_consensus_effects chain effects =
  (* effects are added to the top, so handle them reversed *)
  let rev_effects = List.rev effects in
  List.fold_left
    (fun (chain, effects) effect ->
      let chain, effect = handle_consensus_effect chain effect in
      match effect with
      | Some effect -> (chain, effect :: effects)
      | None -> (chain, effects))
    (chain, []) rev_effects

let incoming_block ~current ~block chain =
  let (Chain { pool; protocol; consensus; producer }) = chain in
  let consensus, effects = Consensus.incoming_block ~current ~block consensus in
  let chain = Chain { pool; protocol; consensus; producer } in
  handle_consensus_effects chain effects

let incoming_signature ~current ~signature chain =
  let (Chain { pool; protocol; consensus; producer }) = chain in
  let consensus, effects =
    Consensus.incoming_signature ~current ~signature consensus
  in
  let chain = Chain { pool; protocol; consensus; producer } in
  handle_consensus_effects chain effects

let incoming_timeout ~current chain =
  let (Chain { pool; protocol; consensus; producer }) = chain in
  let effects =
    let (Consensus { block_pool = _; signer = _; state }) = consensus in
    match Producer.produce ~current ~state producer with
    | Some block -> [ Broadcast_block { block } ]
    | None -> []
  in
  (Chain { pool; protocol; consensus; producer }, effects)

let incoming_operation ~operation node =
  let (Chain { pool; protocol; consensus; producer }) = node in
  let producer = Producer.incoming_operation ~operation producer in
  Chain { pool; protocol; consensus; producer }
