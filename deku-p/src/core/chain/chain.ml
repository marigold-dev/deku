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
      oldest_trusted : Level.t;
      trusted : Message.Network.t Level.Map.t;
    }

and t = chain [@@deriving yojson]

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
  | Fragment_store of { block : Block.t; votes : Verified_signature.Set.t }

type outcome =
  | Outcome_gossip of { outcome : Gossip.outcome }
  | Outcome_produce of { block : Block.t }
  | Outcome_apply of {
      block : Block.t;
      votes : Verified_signature.t Key_hash.Map.t;
      protocol : Protocol.t;
      receipts : Receipt.t list;
    }
  | Outcome_store of { level : Level.t; network : Message.Network.t }

type action =
  | Chain_timeout of { until : Timestamp.t }
  | Chain_broadcast of { raw_header : string; raw_content : string }
  | Chain_send_message of {
      connection : Connection_id.t; [@opaque]
      raw_header : string;
      raw_content : string;
    }
  | Chain_send_request of { raw_header : string; raw_content : string }
  | Chain_fragment of { fragment : fragment [@opaque] }
  | Chain_save_block of { block : Block.t }
  | Chain_commit of {
      current_level : Level.t;
      payload_hash : BLAKE2b.t;
      state_root_hash : BLAKE2b.t;
      signatures : (Key.t * Signature.t) option list;
      validators : Key_hash.t list;
      withdrawal_handles_hash : Deku_ledger.Ledger.Withdrawal_handle.hash;
    }
[@@deriving show]

(* helpers *)
let rec drop ~level ~until by_level =
  match Level.(until > level) with
  | true ->
      let by_level = Level.Map.remove level by_level in
      let level = Level.next level in
      drop ~level ~until by_level
  | false -> Level.Map.remove until by_level

let drop ~until by_level =
  match Level.Map.min_binding_opt by_level with
  | Some (level, _by_hash) -> drop ~level ~until by_level
  | None -> by_level

let make ~validators ~vm_state =
  let gossip = Gossip.initial in
  let validators = Validators.of_key_hash_list validators in
  let protocol = Protocol.initial_with_vm_state ~vm_state in
  let consensus = Consensus.make ~validators in
  let producer = Producer.empty in
  let oldest_trusted = Level.zero in
  let trusted = Level.Map.empty in
  Chain { gossip; protocol; consensus; producer; oldest_trusted; trusted }

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
  | Consensus_timeout { until } -> (chain, [ Chain_timeout { until } ])
  | Consensus_produce { above } ->
      let (Chain { protocol; producer; _ }) = chain in
      let (Protocol { ledger; _ }) = protocol in
      let withdrawal_handles_hash =
        Deku_ledger.Ledger.withdrawal_handles_root_hash ledger
      in
      let fragment =
        Fragment_produce { producer; above; withdrawal_handles_hash }
      in
      (chain, [ Chain_fragment { fragment } ])
  | Consensus_vote { level; vote } ->
      let content = Message.Content.vote ~level ~vote in
      let fragment = Gossip.broadcast_message ~content in
      let fragment = Fragment_gossip { fragment } in
      (chain, [ Chain_fragment { fragment } ])
  | Consensus_apply { block; votes } ->
      (* TODO: restarting here is weird and probably half buggy *)
      let (Chain { protocol; _ }) = chain in
      (* let () =
           let (Block { level; _ }) = block in
           Format.printf "trusted(%.3f): %a\n%!" (Unix.gettimeofday ()) Level.pp
             level
         in *)
      let apply = Fragment_apply { block; votes; protocol } in
      let apply = Chain_fragment { fragment = apply } in
      let store = Fragment_store { block; votes } in
      let store = Chain_fragment { fragment = store } in
      (chain, [ apply; store ])
  | Consensus_request { above } ->
      let (Chain ({ gossip; _ } as chain)) = chain in
      let gossip, network = Gossip.send_request ~above gossip in
      let chain = Chain { chain with gossip } in
      let actions =
        match network with
        | Some network ->
            let (Network_request { raw_header; raw_content }) = network in
            [ Chain_send_request { raw_header; raw_content } ]
        | None -> []
      in
      (chain, actions)

let apply_consensus_actions chain consensus_actions =
  List.fold_left
    (fun (chain, actions) consensus_action ->
      let chain, additional = apply_consensus_action chain consensus_action in
      (chain, additional @ actions))
    (chain, []) consensus_actions

(* core *)
let incoming_block ~identity ~current ~block chain =
  Logs.info (fun m -> m "Incoming block %a" Block.pp block);
  let (Chain ({ consensus; _ } as chain)) = chain in
  let consensus, actions =
    Consensus.incoming_block ~identity ~current ~block consensus
  in
  let chain = Chain { chain with consensus } in
  apply_consensus_actions chain actions

let incoming_vote ~current ~level ~vote chain =
  (* let () =
       let key_hash = Verified_signature.key_hash vote in
       Logs.info (fun m -> m "incoming.vote(%.3f): %s\n%!" (Unix.gettimeofday ())
         (Deku_crypto.Key_hash.to_b58 key_hash))
     in *)
  let (Chain ({ consensus; _ } as chain)) = chain in
  let consensus, actions =
    Consensus.incoming_vote ~current ~level ~vote consensus
  in
  let chain = Chain { chain with consensus } in
  apply_consensus_actions chain actions

let incoming_operation ~operation chain =
  Logs.info (fun m -> m "Incoming operation: %a" Operation.Signed.pp operation);
  let (Chain ({ producer; _ } as chain)) = chain in
  let producer = Producer.incoming_operation ~operation producer in
  let chain = Chain { chain with producer } in
  (chain, [])

let incoming_tezos_operation ~tezos_operation chain =
  let (Chain ({ producer; _ } as chain)) = chain in
  let producer = Producer.incoming_tezos_operation ~tezos_operation producer in
  (Chain { chain with producer }, [])

let incoming_message ~identity ~current ~content chain =
  let open Message.Content in
  match content with
  | Content_block block -> incoming_block ~identity ~current ~block chain
  | Content_vote { level; vote } -> incoming_vote ~current ~level ~vote chain
  | Content_operation operation -> incoming_operation ~operation chain
  | Content_accepted { block; votes } ->
      let (Block { level; _ }) = block in
      let chain, actions = incoming_block ~identity ~current ~block chain in
      Logs.info (fun m -> m "accepted: %a\n%!" Level.pp level);
      List.fold_left
        (fun (chain, actions) vote ->
          let chain, additional = incoming_vote ~current ~level ~vote chain in
          (* TODO: I don't like this @ *)
          (chain, additional @ actions))
        (chain, actions) votes

let incoming_request ~connection ~above chain =
  let (Chain { trusted; _ }) = chain in
  let rev_messages =
    Level.Map.fold
      (fun level message messages ->
        match Level.(level > above) with
        | true -> message :: messages
        | false -> messages)
      trusted []
  in
  let actions =
    List.rev_map
      (fun network ->
        let open Message.Network in
        let (Network_message { raw_header; raw_content }) = network in
        Chain_send_message { connection; raw_header; raw_content })
      rev_messages
  in
  (chain, actions)

let apply_gossip_action ~identity ~current ~gossip_action chain =
  match gossip_action with
  | Gossip.Gossip_apply_and_broadcast { content; network } ->
      let chain, actions = incoming_message ~identity ~current ~content chain in
      let broadcast =
        let (Network_message { raw_header; raw_content }) = network in
        Chain_broadcast { raw_header; raw_content }
      in
      let actions = broadcast :: actions in
      (chain, actions)
  | Gossip.Gossip_send_message { connection; network } ->
      let send =
        let (Network_message { raw_header; raw_content }) = network in
        Chain_send_message { connection; raw_header; raw_content }
      in
      (chain, [ send ])
  | Gossip.Gossip_incoming_request { connection; above } ->
      incoming_request ~connection ~above chain
  | Gossip.Gossip_fragment { fragment } ->
      let fragment = Fragment_gossip { fragment } in
      let fragment = Chain_fragment { fragment } in
      (chain, [ fragment ])

(* external *)
let incoming ~raw_header ~raw_content chain =
  let (Chain ({ gossip; _ } as chain)) = chain in
  let gossip, fragment =
    Gossip.incoming_message ~raw_header ~raw_content gossip
  in
  let fragment =
    match fragment with
    | Some fragment -> Some (Fragment_gossip { fragment })
    | None -> None
  in
  let chain = Chain { chain with gossip } in
  (chain, fragment)

let request ~connection ~raw_header ~raw_content =
  let fragment = Gossip.incoming_request ~connection ~raw_header ~raw_content in
  Fragment_gossip { fragment }

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

let apply_protocol_apply ~identity ~current ~block ~votes ~protocol ~receipts
    chain =
  let (Chain ({ consensus; producer; _ } as chain)) = chain in
  match Consensus.finished ~identity ~current ~block consensus with
  | Ok (consensus, actions) ->
      (* TODO: make this parallel *)
      let (Block { tezos_operations; _ }) = block in
      let producer = Producer.clean ~receipts ~tezos_operations producer in
      let chain = Chain { chain with protocol; consensus; producer } in

      (* FIXME: need a time-based procedure for tezos commits, not block-based *)
      (* FIXME: rediscuss the need to commit the previous block instead *)
      (* FIXME: validators have to watch when commit did not happen *)
      let (Consensus { validators; _ }) = consensus in
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
      Logs.warn (fun m -> m "chain: no pending block\n%!");
      (Chain chain, [])
  | Error `Wrong_pending_block ->
      Logs.warn (fun m -> m "chain: wrong pending block\n%!");
      (Chain chain, [])

let apply_store_outcome ~level ~network chain =
  let (Chain ({ gossip; oldest_trusted; trusted; _ } as chain)) = chain in
  (* TODO: detect if already trusted? *)
  (* TODO: is this the right place? *)
  let gossip = Gossip.close ~until:level gossip in
  let trusted = Level.Map.add level network trusted in
  let oldest_trusted, trusted =
    let open Deku_constants in
    let level_n = Level.to_n level in
    let level_int = Z.to_int (N.to_z level_n) in
    let trusted_cycle_int = Z.to_int (N.to_z trusted_cycle) in
    (* TODO: this is a workaround *)
    match level_int mod trusted_cycle_int = 0 && level_int <> 0 with
    | true ->
        let trusted = drop ~until:oldest_trusted trusted in
        let oldest_trusted = N.(level_n + trusted_cycle) in
        let oldest_trusted = Level.of_n oldest_trusted in
        (oldest_trusted, trusted)
    | false -> (oldest_trusted, trusted)
  in
  let broadcast =
    let (Network_message { raw_header; raw_content }) = network in
    Chain_broadcast { raw_header; raw_content }
  in
  (Chain { chain with gossip; oldest_trusted; trusted }, [ broadcast ])

let apply ~identity ~current ~outcome chain =
  match outcome with
  | Outcome_gossip { outcome } ->
      apply_gossip_outcome ~identity ~current ~outcome chain
  | Outcome_produce { block } -> apply_protocol_produce ~block chain
  | Outcome_apply { block; votes; protocol; receipts } ->
      apply_protocol_apply ~identity ~current ~block ~votes ~protocol ~receipts
        chain
  | Outcome_store { level; network } ->
      apply_store_outcome ~level ~network chain

let compute ~identity ~default_block_size fragment =
  (* TODO: identity parameter here not ideal *)
  match fragment with
  | Fragment_gossip { fragment } ->
      let outcome = Gossip.compute fragment in
      Outcome_gossip { outcome }
  | Fragment_produce { producer; above; withdrawal_handles_hash } ->
      let block =
        Producer.produce ~identity ~default_block_size ~above
          ~withdrawal_handles_hash producer
      in
      Outcome_produce { block }
  | Fragment_apply { protocol; votes; block } ->
      let (Block { level; payload; tezos_operations; _ }) = block in
      Logs.info (fun m ->
          m "Applying level %a at time %.3f" Level.pp level
            (Unix.gettimeofday ()));
      let payload =
        let (Payload payload) = Payload.decode ~payload in
        Protocol.prepare ~parallel:Parallel.filter_map_p ~payload
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
      (* TODO: this is a workaround *)
      let () = Gc.major () in
      let () =
        let level = Level.to_n level |> N.to_z |> Z.to_int in
        (* TODO: this is a workaround *)
        match level mod 600 = 0 with true -> Gc.compact () | false -> ()
      in
      Outcome_apply { block; votes; protocol; receipts }
  | Fragment_store { block; votes } ->
      (* TODO: problem here is that only the initial 2/3 of votes
         is stored, ideally we should hold more votes *)
      let (Block { level; _ }) = block in
      let votes = Verified_signature.Set.elements votes in
      let content = Message.Content.accepted ~block ~votes in
      let (Message { header = _; content = _; network }) =
        Message.encode ~content
      in
      Outcome_store { level; network }

let reload ~current chain =
  let chain, actions =
    let (Chain ({ consensus; _ } as chain)) = chain in
    let consensus, actions = Consensus.reload ~current consensus in
    let chain = Chain { chain with consensus } in
    apply_consensus_actions chain actions
  in

  let (Chain ({ gossip; consensus; _ } as chain)) = chain in
  let (Block { level = current_level; _ }) =
    Consensus.trusted_block consensus
  in
  let gossip = Gossip.close ~until:current_level gossip in
  (Chain { chain with gossip }, actions)

let test () =
  Eio_main.run @@ fun env ->
  Parallel.Pool.run ~env ~domains:16 @@ fun () ->
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

  let chain = make ~validators ~vm_state:Ocaml_wasm_vm.State.empty in
  let block =
    let (Block { hash = current_block; level = current_level; _ }) =
      Genesis.block
    in
    let level = Level.next current_level in
    let previous = current_block in
    let payload = Payload.Payload [] in
    let tezos_operations = [] in
    let withdrawal_handles_hash = BLAKE2b.hash "tuturu" in
    let block =
      Block.produce ~identity ~level ~previous ~payload ~tezos_operations
        ~withdrawal_handles_hash
    in
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
            | Chain_broadcast { raw_header; raw_content } ->
                let chain, fragment = incoming ~raw_header ~raw_content chain in
                let actions =
                  match fragment with
                  | Some fragment ->
                      let fragment = Chain_fragment { fragment } in
                      [ fragment ]
                  | None -> []
                in
                (chain, actions)
            | Chain_send_message { connection = _; raw_header; raw_content } ->
                let chain, fragment = incoming ~raw_header ~raw_content chain in
                let actions =
                  match fragment with
                  | Some fragment ->
                      let fragment = Chain_fragment { fragment } in
                      [ fragment ]
                  | None -> []
                in
                (chain, actions)
            | Chain_send_request { raw_header; raw_content } ->
                let connection = Connection_id.initial in
                let fragment = request ~connection ~raw_header ~raw_content in
                let fragment = Chain_fragment { fragment } in
                (chain, [ fragment ])
            | Chain_fragment { fragment } ->
                let outcome =
                  Parallel.parallel (fun () ->
                      compute ~identity ~default_block_size:100_000 fragment)
                in
                apply ~identity ~current ~outcome chain
            | Chain_save_block _ -> (chain, [])
            | Chain_commit _ ->
                Printf.eprintf "FIXME: commit not implemented in Chain.test\n%!";
                (chain, [])
          in
          (chain, actions @ additional_actions))
        (chain, []) actions
    in
    loop chain actions
  in
  loop chain actions
