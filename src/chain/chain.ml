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
      trusted : (string * string) Level.Map.t;
    }

and t = chain [@@deriving yojson]

type fragment =
  | Fragment_gossip of { fragment : Gossip.fragment }
  | Fragment_produce of { producer : Producer.t; above : Block.t }
  | Fragment_apply of { protocol : Protocol.t; block : Block.t }
  | Fragment_store of { block : Block.t; votes : Verified_signature.Set.t }

type outcome =
  | Outcome_gossip of { outcome : Gossip.outcome }
  | Outcome_produce of { block : Block.t }
  | Outcome_apply of {
      protocol : Protocol.t;
      block : Block.t;
      receipts : Receipt.t list;
    }
  | Outcome_store of {
      level : Level.t;
      raw_expected_hash : string;
      raw_content : string;
    }

type action =
  | Chain_timeout of { until : Timestamp.t }
  | Chain_broadcast of { raw_expected_hash : string; raw_content : string }
  | Chain_send_message of {
      connection : Connection_id.t;
      raw_expected_hash : string;
      raw_content : string;
    }
  | Chain_send_request of { raw_expected_hash : string; raw_content : string }
  | Chain_fragment of { fragment : fragment }

let make ~validators =
  let gossip = Gossip.empty in
  let validators = Validators.of_key_hash_list validators in
  let protocol = Protocol.initial in
  let consensus = Consensus.make ~validators in
  let producer = Producer.empty in
  let trusted = Level.Map.empty in
  Chain { gossip; protocol; consensus; producer; trusted }

(* after gossip *)
let apply_consensus_action chain consensus_action =
  let open Consensus in
  match consensus_action with
  | Consensus_timeout { until } -> (chain, [ Chain_timeout { until } ])
  | Consensus_produce { above } ->
      let (Chain { producer; _ }) = chain in
      let fragment = Fragment_produce { producer; above } in
      (chain, [ Chain_fragment { fragment } ])
  | Consensus_vote { level; vote } ->
      let content = Message.Content.vote ~level ~vote in
      let fragment = Gossip.broadcast_message ~content in
      let fragment = Fragment_gossip { fragment } in
      (chain, [ Chain_fragment { fragment } ])
  | Consensus_apply { block; votes } ->
      (* TODO: restarting here is weird and probably half buggy *)
      let (Chain { protocol; _ }) = chain in
      let apply = Fragment_apply { protocol; block } in
      let apply = Chain_fragment { fragment = apply } in
      let store = Fragment_store { block; votes } in
      let store = Chain_fragment { fragment = store } in
      (chain, [ apply; store ])
  | Consensus_request { above } ->
      let content = Request.Content.accepted ~above in
      let fragment = Gossip.send_request ~content in
      let fragment = Fragment_gossip { fragment } in
      (chain, [ Chain_fragment { fragment } ])

let apply_consensus_actions chain consensus_actions =
  List.fold_left
    (fun (chain, actions) consensus_action ->
      let chain, additional = apply_consensus_action chain consensus_action in
      (chain, additional @ actions))
    (chain, []) consensus_actions

(* core *)
let incoming_block ~identity ~current ~block chain =
  (* let () =
       let (Block.Block { hash; level; _ }) = block in
       Format.eprintf "incoming.block(%a)(%.3f): %s\n%!" Level.pp level
         (Unix.gettimeofday ()) (Block_hash.to_b58 hash)
     in *)
  let (Chain ({ consensus; _ } as chain)) = chain in
  let consensus, actions =
    Consensus.incoming_block ~identity ~current ~block consensus
  in
  let chain = Chain { chain with consensus } in
  apply_consensus_actions chain actions

let incoming_vote ~current ~level ~vote chain =
  (* let () =
       let key_hash = Verified_signature.key_hash vote in
       Format.eprintf "incoming.vote(%.3f): %s\n%!" (Unix.gettimeofday ())
         (Deku_crypto.Key_hash.to_b58 key_hash)
     in *)
  let (Chain ({ consensus; _ } as chain)) = chain in
  let consensus, actions =
    Consensus.incoming_vote ~current ~level ~vote consensus
  in
  let chain = Chain { chain with consensus } in
  apply_consensus_actions chain actions

let incoming_operation ~operation chain =
  let (Chain ({ producer; _ } as chain)) = chain in
  let producer = Producer.incoming_operation ~operation producer in
  let chain = Chain { chain with producer } in
  (chain, [])

let incoming_message ~identity ~current ~message chain =
  let open Message in
  let (Message { hash = _; content }) = message in
  match content with
  | Content_block block -> incoming_block ~identity ~current ~block chain
  | Content_vote { level; vote } -> incoming_vote ~current ~level ~vote chain
  | Content_operation operation -> incoming_operation ~operation chain
  | Content_accepted { block; votes } ->
      let (Block { level; _ }) = block in
      let chain, actions = incoming_block ~identity ~current ~block chain in
      Format.eprintf "accepted: %a\n%!" Level.pp level;
      List.fold_left
        (fun (chain, actions) vote ->
          let chain, additional = incoming_vote ~current ~level ~vote chain in
          (* TODO: I don't like this @ *)
          (chain, additional @ actions))
        (chain, actions) votes

let incoming_request ~connection ~request chain =
  let open Request in
  let (Chain { trusted; _ }) = chain in
  let (Request { hash = _; content }) = request in
  match content with
  | Content_accepted { above } -> (
      (* TODO: probably single domain is a better idea
         even better would be storing the raw messages per level *)
      (* TODO: send all blocks above *)
      let level = Level.next above in
      match Level.Map.find_opt level trusted with
      | Some (raw_expected_hash, raw_content) ->
          let action =
            Chain_send_message { connection; raw_expected_hash; raw_content }
          in
          (chain, [ action ])
      | None -> (chain, []))

let apply_gossip_action ~identity ~current ~gossip_action chain =
  match gossip_action with
  | Gossip.Gossip_apply_and_broadcast { message; raw_message } ->
      let chain, actions = incoming_message ~identity ~current ~message chain in
      let broadcast =
        let (Raw_message { hash; raw_content }) = raw_message in
        let raw_expected_hash = Message_hash.to_b58 hash in
        Chain_broadcast { raw_expected_hash; raw_content }
      in
      let actions = broadcast :: actions in
      (chain, actions)
  | Gossip.Gossip_send_message { connection; raw_message } ->
      let (Raw_message { hash; raw_content }) = raw_message in
      let raw_expected_hash = Message_hash.to_b58 hash in
      let send =
        Chain_send_message { connection; raw_expected_hash; raw_content }
      in
      (chain, [ send ])
  | Gossip.Gossip_send_request { raw_request } ->
      let (Raw_request { hash; raw_content }) = raw_request in
      let raw_expected_hash = Request_hash.to_b58 hash in
      let send = Chain_send_request { raw_expected_hash; raw_content } in
      (chain, [ send ])
  | Gossip.Gossip_incoming_request { connection; request } ->
      incoming_request ~connection ~request chain
  | Gossip.Gossip_fragment { fragment } ->
      let fragment = Fragment_gossip { fragment } in
      let fragment = Chain_fragment { fragment } in
      (chain, [ fragment ])

(* external *)
let incoming ~raw_expected_hash ~raw_content chain =
  let (Chain ({ gossip; _ } as chain)) = chain in
  let gossip, fragment =
    Gossip.incoming_message ~raw_expected_hash ~raw_content gossip
  in
  let fragment =
    match fragment with
    | Some fragment -> Some (Fragment_gossip { fragment })
    | None -> None
  in
  let chain = Chain { chain with gossip } in
  (chain, fragment)

let request ~connection ~raw_expected_hash ~raw_content =
  match Gossip.incoming_request ~connection ~raw_expected_hash ~raw_content with
  | Some fragment -> Some (Fragment_gossip { fragment })
  | None -> None

let timeout ~identity ~current chain =
  let (Chain ({ consensus; _ } as chain)) = chain in
  let consensus, actions = Consensus.timeout ~identity ~current consensus in
  let chain = Chain { chain with consensus } in
  apply_consensus_actions chain actions

let apply_gossip_outcome ~identity ~current ~outcome chain =
  let (Chain ({ gossip; _ } as chain)) = chain in
  let gossip, gossip_action = Gossip.apply ~outcome gossip in
  let chain = Chain { chain with gossip } in
  match gossip_action with
  | Some gossip_action ->
      apply_gossip_action ~identity ~current ~gossip_action chain
  | None -> (chain, [])

let apply_protocol_produce ~block chain =
  let content = Message.Content.block block in
  let fragment = Gossip.broadcast_message ~content in
  let fragment = Fragment_gossip { fragment } in
  (chain, [ Chain_fragment { fragment } ])

let apply_protocol_apply ~identity ~current ~protocol ~block ~receipts chain =
  let (Chain ({ consensus; producer; _ } as chain)) = chain in
  match Consensus.finished ~identity ~current ~block consensus with
  | Ok (consensus, actions) ->
      (* TODO: make this parallel *)
      let producer = Producer.clean ~receipts producer in
      let chain = Chain { chain with protocol; consensus; producer } in
      apply_consensus_actions chain actions
  | Error `No_pending_block ->
      Format.eprintf "chain: no pending block\n%!";
      (Chain chain, [])
  | Error `Wrong_pending_block ->
      Format.eprintf "chain: wrong pending block\n%!";
      (Chain chain, [])

let apply_store_outcome ~level ~raw_expected_hash ~raw_content chain =
  let (Chain ({ trusted; _ } as chain)) = chain in
  (* TODO: detect if already trusted? *)
  let trusted = Level.Map.add level (raw_expected_hash, raw_content) trusted in
  (Chain { chain with trusted }, [])

let apply ~identity ~current ~outcome chain =
  match outcome with
  | Outcome_gossip { outcome } ->
      apply_gossip_outcome ~identity ~current ~outcome chain
  | Outcome_produce { block } -> apply_protocol_produce ~block chain
  | Outcome_apply { protocol; block; receipts } ->
      apply_protocol_apply ~identity ~current ~protocol ~block ~receipts chain
  | Outcome_store { level; raw_expected_hash; raw_content } ->
      apply_store_outcome ~level ~raw_expected_hash ~raw_content chain

let compute ~identity fragment =
  (* TODO: identity parameter here not ideal *)
  match fragment with
  | Fragment_gossip { fragment } ->
      let outcome = Gossip.compute fragment in
      Outcome_gossip { outcome }
  | Fragment_produce { producer; above } ->
      let block = Producer.produce ~identity ~above producer in
      Outcome_produce { block }
  | Fragment_apply { protocol; block } ->
      let (Block { level; payload; _ }) = block in
      let () =
        Format.printf "%a(%.3f)\n%!" Level.pp level (Unix.gettimeofday ())
      in
      let payload =
        Protocol.prepare ~parallel:(fun f l -> List.filter_map f l) ~payload
      in
      let protocol, receipts =
        Protocol.apply ~current_level:level ~payload protocol
      in
      Outcome_apply { protocol; block; receipts }
  | Fragment_store { block; votes } ->
      (* TODO: problem here is that only the initial 2/3 of votes
         is stored, ideally we should hold more votes *)
      let (Block { level; _ }) = block in
      let votes = Verified_signature.Set.elements votes in
      let content = Message.Content.accepted ~block ~votes in
      let _message, raw_message = Message.encode ~content in
      let (Raw_message { hash; raw_content }) = raw_message in
      let raw_expected_hash = Message_hash.to_b58 hash in
      Outcome_store { level; raw_expected_hash; raw_content }

let clear chain =
  let (Chain ({ gossip; consensus; _ } as chain)) = chain in
  let gossip = Gossip.clear gossip in
  Chain { chain with gossip; consensus }

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

  let chain = make ~validators in
  let block =
    let (Block { hash = current_block; level = current_level; _ }) =
      Genesis.block
    in
    let level = Level.next current_level in
    let previous = current_block in
    let operations = [] in
    let block = Block.produce ~identity ~level ~previous ~operations in
    block
  in

  let chain, actions =
    incoming_block ~identity ~current:(get_current ()) ~block chain
  in

  let rec loop chain actions =
    let current = get_current () in
    let chain, actions =
      List.fold_left
        (fun (chain, actions) action ->
          let chain, additional_actions =
            match action with
            | Chain_timeout _ -> (chain, [])
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
            | Chain_send_message
                { connection = _; raw_expected_hash; raw_content } ->
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
                let connection = Connection_id.initial in
                let fragment =
                  request ~connection ~raw_expected_hash ~raw_content
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
                let outcome = compute ~identity fragment in
                apply ~identity ~current ~outcome chain
          in
          (chain, actions @ additional_actions))
        (chain, []) actions
    in
    loop chain actions
  in
  loop chain actions
