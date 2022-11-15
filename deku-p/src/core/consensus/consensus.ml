open Deku_concepts
open Deku_crypto
open Block

(* TODO: maybe pipeline produce?
        this would allow producing next while applying current *)

(* TODO: this only detects double signed level when its time to apply *)
(* TODO: only check signatures if block is known? *)
type action =
  | Consensus_timeout of { until : Timestamp.t }
  | Consensus_produce of { above : Block.t }
  | Consensus_vote of { level : Level.t; vote : Verified_signature.t }
  (* TODO: maybe consensus should hold the votes *)
  | Consensus_apply of {
      block : Block.t;
      votes : Verified_signature.Set.t; [@opaque]
    }
  | Consensus_request of { above : Level.t }
[@@deriving show]

type state =
  (* normal *)
  | Propose of { finalized : Block.t }
  | Vote of { finalized : Block.t }
  | Apply of { pending : Block.t }
  (* late *)
  | Pending_missing of { finalized : Block.t; accepted : Level.t }
  | Pending_apply of { pending : Block.t; accepted : Level.t }
  (* dead *)
  (* TODO: reason why it is corrupted *)
  | Corrupted_stuck of { finalized : Block.t; clash : Block.t }
  | Corrupted_apply of { pending : Block.t; clash : Block.t }

and consensus =
  | Consensus of {
      validators : Validators.t;
      block_pool : Block_pool.t;
      state : state;
      (* TODO: this variable is weird here *)
      accepted_at : Timestamp.t;
    }

and t = consensus [@@deriving yojson]

let state_encoding =
  let open Data_encoding in
  union
    [
      case ~title:"Propose" (Tag 0) Block.encoding
        (function Propose { finalized } -> Some finalized | _ -> None)
        (fun finalized -> Propose { finalized });
      case ~title:"Vote" (Tag 1) Block.encoding
        (function Vote { finalized } -> Some finalized | _ -> None)
        (fun finalized -> Vote { finalized });
      case ~title:"Apply" (Tag 2) Block.encoding
        (function Apply { pending } -> Some pending | _ -> None)
        (fun pending -> Apply { pending });
      case ~title:"Pending_missing" (Tag 3)
        (tup2 Block.encoding Level.encoding)
        (function
          | Pending_missing { finalized; accepted } -> Some (finalized, accepted)
          | _ -> None)
        (fun (finalized, accepted) -> Pending_missing { finalized; accepted });
      case ~title:"Pending_apply" (Tag 4)
        (tup2 Block.encoding Level.encoding)
        (function
          | Pending_apply { pending; accepted } -> Some (pending, accepted)
          | _ -> None)
        (fun (pending, accepted) -> Pending_apply { pending; accepted });
      case ~title:"Corrupted_stuck" (Tag 5)
        (tup2 Block.encoding Block.encoding)
        (function
          | Corrupted_stuck { finalized; clash } -> Some (finalized, clash)
          | _ -> None)
        (fun (finalized, clash) -> Corrupted_stuck { finalized; clash });
      case ~title:"Corrupted_apply" (Tag 6)
        (tup2 Block.encoding Block.encoding)
        (function
          | Corrupted_apply { pending; clash } -> Some (pending, clash)
          | _ -> None)
        (fun (pending, clash) -> Corrupted_apply { pending; clash });
    ]

let encoding =
  let open Data_encoding in
  conv
    (fun (Consensus { validators; block_pool; state; accepted_at }) ->
      (validators, block_pool, state, accepted_at))
    (fun (validators, block_pool, state, accepted_at) ->
      Consensus { validators; block_pool; state; accepted_at })
    (tup4 Validators.encodings Block_pool.encoding state_encoding
       Timestamp.encoding)

let make ~validators =
  let block_pool = Block_pool.empty in
  let state = Propose { finalized = Genesis.block } in
  let accepted_at = Timestamp.genesis in
  Consensus { validators; block_pool; state; accepted_at }

(* views *)
let trusted_block consensus =
  let (Consensus { state; _ }) = consensus in
  match state with
  | Propose { finalized = trusted }
  | Vote { finalized = trusted }
  | Apply { pending = trusted }
  | Pending_missing { finalized = trusted; accepted = _ }
  | Pending_apply { pending = trusted; accepted = _ }
  | Corrupted_stuck { finalized = trusted; clash = _ }
  | Corrupted_apply { pending = trusted; clash = _ } ->
      trusted

let pending_block consensus =
  let (Consensus { state; _ }) = consensus in
  match state with
  | Apply { pending }
  | Pending_apply { pending; accepted = _ }
  | Corrupted_apply { pending; clash = _ } ->
      Some pending
  | Propose _ | Vote _ | Pending_missing _ | Corrupted_stuck _ -> None

let accepted_level consensus =
  let (Consensus { state; _ }) = consensus in
  match state with
  | Propose { finalized = accepted }
  | Vote { finalized = accepted }
  | Apply { pending = accepted }
  | Corrupted_stuck { finalized = accepted; clash = _ }
  | Corrupted_apply { pending = accepted; clash = _ } ->
      let (Block { level = accepted_level; _ }) = accepted in
      accepted_level
  | Pending_missing { finalized = _; accepted }
  | Pending_apply { pending = _; accepted } ->
      accepted

let next_timeout ~current consensus =
  let (Consensus { accepted_at; _ }) = consensus in
  Timestamp.next_timeout ~current ~since:accepted_at

let is_pending_level ~level consensus =
  let (Block { level = trusted_level; _ }) = trusted_block consensus in
  Level.(level > trusted_level)

let is_validator ~validator consensus =
  let (Consensus { validators; _ }) = consensus in
  Validators.mem validator validators

let find_block ~level ~hash consensus =
  let (Consensus { block_pool; _ }) = consensus in
  Block_pool.find_block ~level ~hash block_pool

let find_votes ~level ~hash consensus =
  let (Consensus { block_pool; _ }) = consensus in
  Block_pool.find_votes ~level ~hash block_pool

let required_signatures consensus =
  let (Consensus { validators; _ }) = consensus in
  let validators = Validators.cardinal validators in
  let validators = Float.of_int validators in
  let required_signatures = Float.floor (validators *. (2.0 /. 3.0)) +. 1.0 in
  Float.to_int required_signatures

let is_signed ~block consensus =
  let (Block { hash; level; _ }) = block in
  let required_signatures = required_signatures consensus in
  let votes = find_votes ~level ~hash consensus in
  let votes = Verified_signature.Set.cardinal votes in
  votes >= required_signatures

(* updates *)
let append_block ~block consensus =
  let (Consensus ({ block_pool; _ } as consensus)) = consensus in
  let block_pool = Block_pool.append_block ~block block_pool in
  Consensus { consensus with block_pool }

let append_vote ~level ~vote consensus =
  let (Consensus ({ block_pool; _ } as consensus)) = consensus in
  let block_pool = Block_pool.append_vote ~level ~vote block_pool in
  Consensus { consensus with block_pool }

let finish_level ~state ~level consensus =
  let (Consensus ({ block_pool; _ } as consensus)) = consensus in
  let block_pool = Block_pool.close_level ~until:level block_pool in
  Consensus { consensus with block_pool; state }

let with_accept_block ~state ~accepted_at consensus =
  let (Consensus consensus) = consensus in
  Consensus { consensus with state; accepted_at }

(* logic *)

let is_expected_level ~trusted_level block =
  let (Block { level; _ }) = block in
  Level.(equal (next trusted_level) level)

let is_expected_previous ~trusted_hash block =
  let (Block { previous; _ }) = block in
  Block_hash.equal trusted_hash previous

let is_expected_author ~current_author block =
  let (Block { author; _ }) = block in
  Key_hash.equal current_author author

let find_author_after ~current ~after consensus =
  let (Consensus { validators; accepted_at; _ }) = consensus in
  let (Block { author = last_author; _ }) = after in
  let skip = Timestamp.timeouts_since ~current ~since:accepted_at in
  Validators.skip ~after:last_author ~skip validators

let try_to_sign_block ~identity ~current ~block consensus =
  let (Consensus ({ validators; state; accepted_at; _ } as consensus)) =
    consensus
  in
  match state with
  | Propose { finalized } -> (
      let (Block
            {
              hash = trusted_hash;
              level = trusted_level;
              author = last_author;
              _;
            }) =
        finalized
      in
      let current_author =
        let skip = Timestamp.timeouts_since ~current ~since:accepted_at in
        Validators.skip ~after:last_author ~skip validators
      in
      match
        is_expected_level ~trusted_level block
        && is_expected_previous ~trusted_hash block
        && is_expected_author ~current_author block
      with
      | true ->
          let state = Vote { finalized } in
          let consensus = Consensus { consensus with state } in
          let vote = Block.sign ~identity block in
          let action =
            let (Block { level; _ }) = block in
            Consensus_vote { level; vote }
          in
          Some (consensus, action)
      | false -> None)
  | Vote _ | Apply _ | Pending_missing _ | Pending_apply _ | Corrupted_stuck _
  | Corrupted_apply _ ->
      None

let try_to_produce ~identity ~current consensus =
  let (Consensus { state; _ }) = consensus in
  let self = Identity.key_hash identity in
  match state with
  | Propose { finalized } -> (
      let current_author =
        find_author_after ~current ~after:finalized consensus
      in
      match Key_hash.equal current_author self with
      | true ->
          let above = trusted_block consensus in
          Some (Consensus_produce { above })
      | false -> None)
  | Vote _ | Apply _ | Pending_missing _ | Pending_apply _ | Corrupted_stuck _
  | Corrupted_apply _ ->
      None

let accept_clash_block ~block consensus =
  let (Consensus { state; accepted_at; _ }) = consensus in
  let state =
    match state with
    | Propose { finalized }
    | Vote { finalized }
    | Pending_missing { finalized; accepted = _ } ->
        Corrupted_stuck { finalized; clash = block }
    | Apply { pending } | Pending_apply { pending; accepted = _ } ->
        Corrupted_apply { pending; clash = block }
    | Corrupted_stuck { finalized; clash } ->
        Corrupted_stuck { finalized; clash }
    | Corrupted_apply { pending; clash } -> Corrupted_apply { pending; clash }
  in
  let consensus = with_accept_block ~state ~accepted_at consensus in
  (consensus, None, None)

let accept_next_block ~current ~block consensus =
  (* if (block = next current) *)
  let (Consensus { state; accepted_at; _ }) = consensus in
  let (Block { hash; level; _ }) = block in

  let accepted_at, timeout =
    let accepted_level = accepted_level consensus in
    match Level.(equal level (next accepted_level)) with
    | true -> (current, true)
    | false -> (accepted_at, false)
  in

  let state, apply =
    match state with
    | Propose _ | Vote _ -> (Apply { pending = block }, true)
    | Apply { pending } -> (Pending_apply { pending; accepted = level }, false)
    | Pending_missing { finalized = _; accepted } -> (
        match Level.equal level accepted with
        | true -> (Apply { pending = block }, true)
        | false -> (Pending_apply { pending = block; accepted }, true))
    | Pending_apply { pending; accepted } ->
        (Pending_apply { pending; accepted }, false)
    | Corrupted_stuck { finalized; clash } ->
        (Corrupted_stuck { finalized; clash }, false)
    | Corrupted_apply { pending; clash } ->
        (Corrupted_apply { pending; clash }, false)
  in
  let consensus = with_accept_block ~state ~accepted_at consensus in
  let apply =
    match apply with
    | true ->
        let votes = find_votes ~level ~hash consensus in
        Some (Consensus_apply { block; votes })
    | false -> None
  in
  let timeout =
    match timeout with
    | true ->
        let next_timeout = next_timeout ~current consensus in
        Some (Consensus_timeout { until = next_timeout })
    | false -> None
  in
  (consensus, apply, timeout)

let accept_future_block ~current ~block consensus =
  (* if (block > accepted) *)
  let (Consensus { state; _ }) = consensus in
  let (Block { level; _ }) = block in
  let accepted = level in
  let state =
    match state with
    | Propose { finalized } | Vote { finalized } ->
        Pending_missing { finalized; accepted }
    | Apply { pending } -> Pending_apply { pending; accepted }
    | Pending_missing { finalized; accepted = _ } ->
        Pending_missing { finalized; accepted }
    | Pending_apply { pending; accepted = _ } ->
        Pending_apply { pending; accepted }
    | Corrupted_stuck { finalized; clash } ->
        Corrupted_stuck { finalized; clash }
    | Corrupted_apply { pending; clash } -> Corrupted_apply { pending; clash }
  in
  let accepted_at = current in
  let consensus = with_accept_block ~state ~accepted_at consensus in
  let request =
    match state with
    | Pending_missing { finalized = current; accepted = _ } ->
        let (Block { level; _ }) = current in
        Some (Consensus_request { above = level })
    | Propose _ | Vote _ | Apply _ | Pending_apply _ | Corrupted_stuck _
    | Corrupted_apply _ ->
        None
  in
  let timeout =
    let next_timeout = next_timeout ~current consensus in
    Some (Consensus_timeout { until = next_timeout })
  in
  (consensus, request, timeout)

let accept_future_block ~current ~block consensus =
  let (Block { level; _ }) = block in
  let accepted = accepted_level consensus in
  match Level.(level > accepted) with
  | true -> accept_future_block ~current ~block consensus
  | false -> (consensus, None, None)

let accept_block ~current ~block consensus =
  let (Block { hash = trusted_hash; level = trusted_level; _ }) =
    trusted_block consensus
  in
  match is_expected_level ~trusted_level block with
  | true -> (
      match is_expected_previous ~trusted_hash block with
      | false -> accept_clash_block ~block consensus
      | true -> accept_next_block ~current ~block consensus)
  | false -> accept_future_block ~current ~block consensus

let incoming_block_or_vote ~current ~block consensus =
  match is_signed ~block consensus with
  | true -> accept_block ~current ~block consensus
  | false -> (consensus, None, None)

let incoming_block ~identity ~current ~block consensus =
  let (Block { level; _ }) = block in
  match is_pending_level ~level consensus with
  | true ->
      let consensus = append_block ~block consensus in
      let consensus, vote =
        match try_to_sign_block ~identity ~current ~block consensus with
        | Some (consensus, vote) -> (consensus, Some vote)
        | None -> (consensus, None)
      in
      let actions = match vote with Some vote -> [ vote ] | None -> [] in

      let consensus, action, timeout =
        incoming_block_or_vote ~current ~block consensus
      in
      let actions =
        match action with Some action -> action :: actions | None -> actions
      in
      let actions =
        match timeout with
        | Some timeout -> timeout :: actions
        | None -> actions
      in

      (consensus, actions)
  | false -> (consensus, [])

let incoming_vote ~current ~level ~vote consensus =
  let validator = Verified_signature.key_hash vote in
  let hash = Verified_signature.signed_hash vote in
  let hash = Block_hash.of_blake2b hash in

  match
    is_pending_level ~level consensus && is_validator ~validator consensus
  with
  | true -> (
      let consensus = append_vote ~level ~vote consensus in
      match find_block ~level ~hash consensus with
      | Some block ->
          let consensus, action, timeout =
            incoming_block_or_vote ~current ~block consensus
          in
          let actions =
            match action with Some action -> [ action ] | None -> []
          in
          let actions =
            match timeout with
            | Some timeout -> timeout :: actions
            | None -> actions
          in
          (consensus, actions)
      | None -> (consensus, []))
  | false -> (consensus, [])

let try_next_blocks ~identity ~current consensus =
  let (Consensus { block_pool; _ }) = consensus in
  let (Block { level = trusted_level; _ }) = trusted_block consensus in
  let level = Level.next trusted_level in
  let blocks = Block_pool.find_level ~level block_pool in

  (* TODO: this cannot detect double signed level *)
  let vote =
    List.find_map
      (fun block -> try_to_sign_block ~identity ~current ~block consensus)
      blocks
  in
  let consensus, action =
    match vote with
    | Some (consensus, vote) -> (consensus, Some vote)
    | None -> (consensus, None)
  in
  let actions = match action with Some action -> [ action ] | None -> [] in

  let signed_block =
    List.find_opt (fun block -> is_signed ~block consensus) blocks
  in
  let consensus, action, timeout =
    match signed_block with
    | Some block -> accept_block ~current ~block consensus
    | None -> (consensus, None, None)
  in
  let actions =
    match action with Some action -> action :: actions | None -> actions
  in
  let actions =
    match timeout with Some timeout -> timeout :: actions | None -> actions
  in
  (consensus, actions)

let timeout ~identity ~current consensus =
  let (Consensus { state; _ }) = consensus in
  let consensus, actions =
    match state with
    | Propose { finalized } | Vote { finalized } ->
        let action = try_to_produce ~identity ~current consensus in
        let consensus, actions = try_next_blocks ~identity ~current consensus in
        let (Consensus consensus) = consensus in
        let state = Propose { finalized } in
        let consensus = Consensus { consensus with state } in
        let actions =
          match action with Some action -> action :: actions | None -> actions
        in
        (consensus, actions)
    | Apply _ | Pending_missing _ | Pending_apply _ | Corrupted_stuck _
    | Corrupted_apply _ ->
        (consensus, [])
  in
  let timeout =
    let next_timeout = next_timeout ~current consensus in
    Consensus_timeout { until = next_timeout }
  in
  let actions = timeout :: actions in
  (consensus, actions)

let finished ~identity ~current ~block consensus =
  let (Consensus { state; _ }) = consensus in
  (* TODO: not a big fan of bind *)
  let ( let* ) = Result.bind in
  let* () =
    match state with
    | Propose _ | Vote _ | Corrupted_stuck _ | Pending_missing _ ->
        Error `No_pending_block
    | Apply { pending }
    | Pending_apply { pending; accepted = _ }
    | Corrupted_apply { pending; clash = _ } -> (
        match Block.equal block pending with
        | true -> Ok ()
        | false -> Error `Wrong_pending_block)
  in

  match state with
  | Propose _ | Vote _ | Corrupted_stuck _ | Pending_missing _ ->
      Error `No_pending_block
  | Apply { pending } ->
      let (Block { level; _ }) = pending in
      let state = Propose { finalized = pending } in
      let consensus = finish_level ~state ~level consensus in
      let consensus, actions = try_next_blocks ~identity ~current consensus in
      let produce = try_to_produce ~identity ~current consensus in
      let actions =
        match produce with Some action -> action :: actions | None -> actions
      in
      Ok (consensus, actions)
  | Corrupted_apply { pending; clash } ->
      let (Block { level; _ }) = pending in
      let state = Corrupted_stuck { finalized = pending; clash } in
      let consensus = finish_level ~state ~level consensus in
      Ok (consensus, [])
  | Pending_apply { pending; accepted } ->
      let (Block { level; _ }) = pending in
      let state = Pending_missing { finalized = pending; accepted } in
      let consensus = finish_level ~state ~level consensus in
      let consensus, actions = try_next_blocks ~identity ~current consensus in
      let produce = try_to_produce ~identity ~current consensus in
      let actions =
        match produce with Some action -> action :: actions | None -> actions
      in
      Ok (consensus, actions)

let reload ~current consensus =
  let (Block { level = current_level; _ }) = trusted_block consensus in
  let (Consensus ({ block_pool; _ } as consensus)) = consensus in
  let block_pool = Block_pool.close_level ~until:current_level block_pool in
  let consensus = Consensus { consensus with block_pool } in
  let timeout =
    let next_timeout = next_timeout ~current consensus in
    Consensus_timeout { until = next_timeout }
  in
  let apply =
    match pending_block consensus with
    | Some block ->
        let (Block { hash; level; _ }) = block in
        let votes = find_votes ~level ~hash consensus in
        Some (Consensus_apply { block; votes })
    | None -> None
  in
  let actions =
    match apply with Some apply -> [ timeout; apply ] | None -> [ timeout ]
  in
  (consensus, actions)

let test () =
  let open Deku_crypto in
  let get_current () = Timestamp.of_float (Unix.gettimeofday ()) in
  let identity () =
    let secret = Ed25519.Secret.generate () in
    let secret = Secret.Ed25519 secret in
    Identity.make secret
  in
  let identity = identity () in
  let validators =
    let validator = Identity.key_hash identity in
    Validators.of_key_hash_list [ validator ]
  in
  let consensus = make ~validators in

  let produce_on_top ~above =
    let (Block { hash = current_block; level = current_level; _ }) = above in
    let level = Level.next current_level in
    let previous = current_block in
    let payload = Payload.Payload [] in
    let tezos_operations = [] in
    let withdrawal_handles_hash = BLAKE2b.hash "tuturu" in
    Block.produce ~identity ~level ~previous ~payload ~tezos_operations
      ~withdrawal_handles_hash
  in

  let block = produce_on_top ~above:Genesis.block in
  let consensus, actions =
    incoming_block ~identity ~current:(get_current ()) ~block consensus
  in
  let rec loop next consensus actions =
    match actions with
    | [] -> loop [] consensus next
    | action :: actions ->
        let current = get_current () in
        let consensus, additional =
          match action with
          | Consensus_timeout _ -> (consensus, [])
          | Consensus_produce { above } ->
              let block = produce_on_top ~above in
              incoming_block ~identity ~current ~block consensus
          | Consensus_vote { level; vote } ->
              incoming_vote ~current ~level ~vote consensus
          | Consensus_apply { block; votes = _ } -> (
              let (Block { level; _ }) = block in
              Logs.info (fun m -> m "%a" Level.pp level);
              match finished ~identity ~current ~block consensus with
              | Ok (consensus, actions) -> (consensus, actions)
              | Error `No_pending_block -> failwith "no pending block"
              | Error `Wrong_pending_block -> failwith "wrong pending block")
          | Consensus_request _ -> assert false
        in
        loop (additional @ next) consensus actions
  in
  loop [] consensus actions
