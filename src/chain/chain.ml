open Deku_concepts
open Deku_protocol
open Deku_consensus
open Deku_crypto
open Deku_gossip
open Deku_stdlib

type chain =
  | Chain of {
      gossip : Gossip.t;
      protocol : Protocol.t;
      consensus : Consensus.t;
      producer : Producer.t;
      applied : (Block.t * Verified_signature.t list) Level.Map.t;
    }

type t = chain

type fragment =
  | Fragment_gossip of { fragment : Gossip.fragment }
  | Fragment_produce of {
      producer : Producer.t;
      above : Block.t;
      withdrawal_handles_hash : BLAKE2b.t;
    }
  | Fragment_apply of {
      (* TODO: votes here is weird, only happens to commit after fragment *)
      block : Block.t;
      votes : Verified_signature.Set.t;
      protocol : Protocol.t;
    }

type outcome =
  | Outcome_gossip of { outcome : Gossip.outcome }
  | Outcome_produce of { block : Block.t }
  | Outcome_apply of {
      block : Block.t;
      votes : Verified_signature.t Key_hash.Map.t;
      protocol : Protocol.t;
      receipts : Receipt.t list;
    }

type action =
  | Chain_timeout of { from : Timestamp.t }
  | Chain_broadcast of { raw_expected_hash : string; raw_content : string }
  | Chain_send_message of {
      connection : Connection_id.t; [@opaque]
      raw_expected_hash : string;
      raw_content : string;
    }
  | Chain_send_request of { raw_expected_hash : string; raw_content : string }
  | Chain_fragment of { fragment : fragment [@opaque] }
  | Chain_save_block of { block : Block.t }
  | Chain_commit of {
      current_level : Level.t;
      payload_hash : BLAKE2b.t;
      state_root_hash : BLAKE2b.t;
      signatures : (Key.t * Signature.t) option list;
      validators : Key_hash.t list;
      withdrawal_handles_hash : Deku_protocol.Ledger.Withdrawal_handle.hash;
    }
[@@deriving show]

let make ~identity ~validators ~default_block_size ~vm_state =
  let gossip = Gossip.empty in
  let validators = Validators.of_key_hash_list validators in
  let protocol = Protocol.initial_with_vm_state ~vm_state in
  let consensus = Consensus.make ~identity ~validators in
  let producer = Producer.make ~identity ~default_block_size in
  let applied = Level.Map.empty in
  Chain { gossip; protocol; consensus; producer; applied }

let commit ~current_level ~block ~votes ~validators =
  let Block.(Block { payload_hash; _ }) = block in
  let state_root_hash =
    Deku_crypto.BLAKE2b.hash "FIXME: we need to add the state root"
  in
  Logs.info (fun m -> m "Commiting block: %a" Block.pp block);
  let (Block.Block { withdrawal_handles_hash; _ }) = block in
  let signatures =
    List.map
      (fun key_hash ->
        match Key_hash.Map.find_opt key_hash votes with
        | Some verified ->
            Some
              ( Verified_signature.key verified,
                Verified_signature.signature verified )
        | None -> None)
      validators
  in
  Chain_commit
    {
      current_level;
      payload_hash;
      state_root_hash;
      signatures;
      validators;
      withdrawal_handles_hash;
    }

(* after gossip *)
let apply_consensus_action chain consensus_action =
  let open Consensus in
  Logs.debug (fun m ->
      m "Chain: applying consensus action: %a" Consensus.pp_action
        consensus_action);
  match consensus_action with
  | Consensus_timeout { from } -> (chain, Chain_timeout { from })
  | Consensus_produce { above } ->
      let (Chain { protocol; producer; _ }) = chain in
      let (Protocol { ledger; _ }) = protocol in
      let withdrawal_handles_hash =
        Ledger.withdrawal_handles_root_hash ledger
      in
      let fragment =
        Fragment_produce { producer; above; withdrawal_handles_hash }
      in
      (chain, Chain_fragment { fragment })
  | Consensus_vote { level; vote } ->
      let content = Message.Content.vote ~level ~vote in
      let fragment = Gossip.broadcast_message ~content in
      let fragment = Fragment_gossip { fragment } in
      (chain, Chain_fragment { fragment })
  | Consensus_apply { block; votes } ->
      (* TODO: restarting here is weird and probably half buggy *)
      let (Chain ({ protocol; applied; _ } as chain)) = chain in
      let (Block { level; _ }) = block in
      let chain =
        (* TODO: problem here is that only the initial 2/3 of votes
            is stored, ideally we should hold more votes *)
        let votes = Verified_signature.Set.elements votes in
        (* TODO: detect if already applied? *)
        let applied = Level.Map.add level (block, votes) applied in
        Chain { chain with applied }
      in
      let fragment = Fragment_apply { block; votes; protocol } in
      (chain, Chain_fragment { fragment })
  | Consensus_request { above } ->
      let content = Request.Content.accepted ~above in
      let fragment = Gossip.send_request ~content in
      let fragment = Fragment_gossip { fragment } in
      (chain, Chain_fragment { fragment })

let apply_consensus_actions chain consensus_actions =
  List.fold_left
    (fun (chain, actions) consensus_action ->
      let chain, action = apply_consensus_action chain consensus_action in
      (chain, action :: actions))
    (chain, []) consensus_actions

(* core *)
let incoming_block ~current ~block chain =
  Logs.info (fun m -> m "Incoming block %a" Block.pp block);
  let (Chain ({ consensus; _ } as chain)) = chain in
  let consensus, actions = Consensus.incoming_block ~current ~block consensus in
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
  Logs.info (fun m -> m "Incoming operation: %a" Operation.pp operation);
  let (Chain ({ producer; _ } as chain)) = chain in
  let producer = Producer.incoming_operation ~operation producer in
  let chain = Chain { chain with producer } in
  (chain, [])

let incoming_tezos_operation ~tezos_operation chain =
  let (Chain { gossip; protocol; consensus; producer; applied }) = chain in
  let producer = Producer.incoming_tezos_operation ~tezos_operation producer in
  (Chain { gossip; protocol; consensus; producer; applied }, [])

let incoming_message ~current ~message chain =
  let open Message in
  let (Message { hash = _; content }) = message in
  match content with
  | Content_block block -> incoming_block ~current ~block chain
  | Content_vote { level; vote } -> incoming_vote ~current ~level ~vote chain
  | Content_operation operation -> incoming_operation ~operation chain
  | Content_accepted { block; votes } ->
      let (Block { level; _ }) = block in
      let chain, actions = incoming_block ~current ~block chain in
      Format.eprintf "accepted: %a\n%!" Level.pp level;
      List.fold_left
        (fun (chain, actions) vote ->
          let chain, additional = incoming_vote ~current ~level ~vote chain in
          (* TODO: I don't like this @ *)
          (chain, additional @ actions))
        (chain, actions) votes

let incoming_request ~connection ~request chain =
  let open Request in
  let (Chain { applied; _ }) = chain in
  let (Request { hash = _; content }) = request in
  match content with
  | Content_accepted { above } -> (
      (* TODO: probably single domain is a better idea
         even better would be storing the raw messages per level *)
      (* TODO: send all blocks above *)
      let level = Level.next above in
      match Level.Map.find_opt level applied with
      | Some (block, votes) ->
          let content = Message.Content.accepted ~block ~votes in
          let fragment = Gossip.send_message ~connection ~content in
          let fragment = Fragment_gossip { fragment } in
          let fragment = Chain_fragment { fragment } in
          (chain, [ fragment ])
      | None -> (chain, []))

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

let timeout ~current chain =
  let (Chain ({ consensus; _ } as chain)) = chain in
  let consensus, actions = Consensus.timeout ~current consensus in
  let chain = Chain { chain with consensus } in
  apply_consensus_actions chain actions

let apply_gossip_outcome ~current ~outcome chain =
  let (Chain ({ gossip; _ } as chain)) = chain in
  let gossip, gossip_action = Gossip.apply ~outcome gossip in
  let chain = Chain { chain with gossip } in
  match gossip_action with
  | Some gossip_action -> apply_gossip_action ~current ~gossip_action chain
  | None -> (chain, [])

let apply_protocol_produce ~block chain =
  let content = Message.Content.block block in
  let fragment = Gossip.broadcast_message ~content in
  let fragment = Fragment_gossip { fragment } in
  (chain, [ Chain_fragment { fragment } ])

let apply_protocol_apply ~current ~block ~votes ~protocol ~receipts chain =
  let (Chain ({ consensus; producer; _ } as chain)) = chain in
  match Consensus.finished ~current ~block consensus with
  | Ok (consensus, actions) ->
      (* TODO: make this parallel *)
      let (Block { tezos_operations; _ }) = block in
      let producer = Producer.clean ~receipts ~tezos_operations producer in
      let chain = Chain { chain with protocol; consensus; producer } in

      (* FIXME: need a time-based procedure for tezos commits, not block-based *)
      (* FIXME: rediscuss the need to commit the previous block instead *)
      (* FIXME: validators have to watch when commit did not happen *)
      let (Consensus { identity; validators; _ }) = consensus in
      let current_block = Consensus.trusted_block consensus in
      let (Block.Block { level = current_level; author = last_block_author; _ })
          =
        current_block
      in

      (* TODO: only the producer should commit on Tezos *)
      let chain, actions = apply_consensus_actions chain actions in
      let actions = Chain_save_block { block } :: actions in
      let actions =
        let level = Level.to_n current_level |> N.to_z |> Z.to_int in
        let self = Identity.key_hash identity in
        match level mod 15 = 0 && Key_hash.equal self last_block_author with
        | true ->
            let validators = Validators.to_key_hash_list validators in
            commit ~current_level ~block ~votes ~validators :: actions
        | false -> actions
      in
      (chain, actions)
  | Error `No_pending_block ->
      Format.eprintf "chain: no pending block\n%!";
      (Chain chain, [])
  | Error `Wrong_pending_block ->
      Format.eprintf "chain: wrong pending block\n%!";
      (Chain chain, [])

let apply ~current ~outcome chain =
  match outcome with
  | Outcome_gossip { outcome } -> apply_gossip_outcome ~current ~outcome chain
  | Outcome_produce { block } -> apply_protocol_produce ~block chain
  | Outcome_apply { block; votes; protocol; receipts } ->
      apply_protocol_apply ~current ~block ~votes ~protocol ~receipts chain

let compute fragment =
  match fragment with
  | Fragment_gossip { fragment } ->
      let outcome = Gossip.compute fragment in
      Outcome_gossip { outcome }
  | Fragment_produce { producer; above; withdrawal_handles_hash } ->
      let block =
        Producer.produce ~parallel_map:List.map ~above ~withdrawal_handles_hash
          producer
      in
      Outcome_produce { block }
  | Fragment_apply { protocol; votes; block } ->
      let (Block { level; payload; tezos_operations; _ }) = block in
      let () =
        Format.printf "%a(%.3f)\n%!" Level.pp level (Unix.gettimeofday ())
      in
      let payload =
        Protocol.prepare ~parallel:(fun f l -> List.filter_map f l) ~payload
      in
      let protocol, receipts, errors =
        Protocol.apply ~current_level:level ~payload protocol ~tezos_operations
      in
      (* TODO: why not log in place? *)
      List.iter
        (fun error ->
          Logs.warn (fun m ->
              m "Error while applying block: %s" (Printexc.to_string error)))
        errors;
      let votes =
        Verified_signature.Set.fold
          (fun vote map ->
            let key_hash = Verified_signature.key_hash vote in
            Key_hash.Map.add key_hash vote map)
          votes Key_hash.Map.empty
      in
      Outcome_apply { block; votes; protocol; receipts }

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

  let chain =
    make ~identity ~validators ~default_block_size:0
      ~vm_state:Deku_external_vm.External_vm_protocol.State.empty
  in
  let block =
    let (Block { hash = current_block; level = current_level; _ }) =
      Genesis.block
    in
    let level = Level.next current_level in
    let previous = current_block in
    let operations = [] in
    let tezos_operations = [] in
    let withdrawal_handles_hash = BLAKE2b.hash "tuturu" in
    let block =
      Block.produce ~parallel_map:List.map ~identity ~level ~previous
        ~operations ~tezos_operations ~withdrawal_handles_hash
    in
    block
  in

  let chain, actions = incoming_block ~current:(get_current ()) ~block chain in

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
                let outcome = compute fragment in
                apply ~current ~outcome chain
            | Chain_save_block _ -> (chain, [])
            | Chain_commit _ ->
                Printf.eprintf "FIXME: commit not implemented in Chain.test";
                (chain, [])
          in
          (chain, actions @ additional_actions))
        (chain, []) actions
    in
    loop chain actions
  in
  loop chain actions
