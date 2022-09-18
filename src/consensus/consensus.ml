open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Block

type action =
  (* protocol *)
  | Consensus_accepted_block of { block : Block.t }
  (* timer *)
  | Consensus_trigger_timeout of { level : Level.t }
  (* network *)
  | Consensus_broadcast_vote of { vote : Verified_signature.t }
  (* TODO: this is clearly hackish *)
  | Consensus_request_block of { self : Key_hash.t; hash : Block_hash.t }

type consensus =
  | Consensus of {
      (* state *)
      current_block : Block.t;
      last_update : Timestamp.t option;
      (* consensus *)
      identity : Identity.t;
      validators : Validators.t;
      accepted : Block_hash.Set.t;
      block_pool : Block_pool.t;
    }

type t = consensus

let make ~identity ~validators =
  let current_block = Genesis.block in
  let last_update = None in
  let accepted = Block_hash.Set.empty in
  let block_pool = Block_pool.empty in
  Consensus
    { current_block; last_update; identity; accepted; block_pool; validators }

let is_expected_level ~current_level block =
  let (Block { level; _ }) = block in
  Level.(equal (next current_level) level)

let is_expected_previous ~current_block block =
  let (Block { previous; _ }) = block in
  Block_hash.equal current_block previous

let is_expected_author ~current_author block =
  let (Block { author; _ }) = block in
  Key_hash.equal current_author author

let current_author ~current consensus =
  let (Consensus { validators; current_block; last_update; _ }) = consensus in
  match last_update with
  | Some last_update ->
      let (Block { author = last_author; _ }) = current_block in
      let skip = Timestamp.timeouts_since ~current ~since:last_update in
      Validators.skip ~after:last_author ~skip validators
  | None -> None

let is_signable ~current ~consensus block =
  let (Consensus { current_block; _ }) = consensus in
  let (Block { hash = current_block; level = current_level; _ }) =
    current_block
  in
  let current_author = current_author ~current consensus in
  match current_author with
  | Some current_author ->
      is_expected_level ~current_level block
      && is_expected_previous ~current_block block
      && is_expected_author ~current_author block
  | None -> false

let required_signatures consensus =
  let (Consensus { validators; _ }) = consensus in
  let validators = Validators.cardinal validators in
  let validators = Float.of_int validators in
  let required_signatures = Float.floor (validators *. (2.0 /. 3.0)) +. 1.0 in
  Float.to_int required_signatures

let is_signed ~block consensus =
  let (Consensus { block_pool; _ }) = consensus in
  let (Block { hash; _ }) = block in
  let required_signatures = required_signatures consensus in
  let votes = Block_pool.find_votes ~hash block_pool in
  let votes = Key_hash.Set.cardinal votes in
  votes >= required_signatures

let is_producer ~current consensus =
  let (Consensus { identity; _ }) = consensus in
  let self = Identity.key_hash identity in
  let current_author = current_author ~current consensus in
  match current_author with
  | Some expected_author -> Key_hash.equal expected_author self
  | None -> (* TODO: this is misleading, false may mean "not sure" *) false

let is_accepted ~block consensus =
  let (Consensus { accepted; _ }) = consensus in
  let (Block { hash; _ }) = block in
  Block_hash.Set.mem hash accepted

let accept_block ~hash consensus =
  let (Consensus ({ accepted; _ } as consensus)) = consensus in
  let accepted = Block_hash.Set.add hash accepted in
  Consensus { consensus with accepted }

let rec incoming_block_or_vote ~current ~block consensus =
  let (Block { hash; previous; level; _ }) = block in
  let consensus =
    match is_signed ~block consensus with
    | true -> accept_block ~hash consensus
    | false -> consensus
  in
  let (Consensus { identity; current_block; block_pool; _ }) = consensus in
  let (Block { hash = current_block; level = current_level; _ }) =
    current_block
  in

  match is_accepted ~block consensus with
  | true -> (
      let consensus = accept_block ~hash:previous consensus in
      match is_expected_level ~current_level block with
      | true -> (
          match is_expected_previous ~current_block block with
          | true -> with_block ~current ~block consensus
          | false ->
              (* this means network was corrupted *)
              failwith "invalid block was accepted")
      | false -> (
          match Level.(current_level < level) with
          | true -> (
              let (Block { previous; _ }) = block in
              match Block_pool.find_block ~hash:previous block_pool with
              | Some previous ->
                  (* TODO: ensure that previous.level = level - 1 *)
                  incoming_block_or_vote ~current ~block:previous consensus
              | None ->
                  let () =
                    let level = Level.to_n level in
                    let level = N.to_z level in
                    Format.eprintf "requesting: %a\n%!" Z.pp_print level
                  in
                  let self = Identity.key_hash identity in
                  let action =
                    Consensus_request_block { self; hash = previous }
                  in
                  (consensus, [ action ]))
          | false ->
              (* old block *)
              let (Consensus ({ block_pool; _ } as consensus)) = consensus in
              let block_pool = Block_pool.remove ~block block_pool in
              let consensus = Consensus { consensus with block_pool } in
              (consensus, [])))
  | false -> (consensus, [])

and with_block ~current ~block consensus =
  let (Consensus { identity; validators; accepted; block_pool; _ }) =
    consensus
  in
  let (Block { hash; level; _ }) = block in
  let () =
    let level = Level.to_n level in
    let level = N.to_z level in
    Format.eprintf "%a\n%!" Z.pp_print level
  in

  let current_block = block in
  let last_update = Some current in
  let block_pool = Block_pool.remove ~block block_pool in
  let consensus =
    Consensus
      { identity; current_block; last_update; validators; accepted; block_pool }
  in

  let next_blocks = Block_pool.find_next ~hash block_pool in

  let consensus, actions =
    Block.Set.fold
      (fun block (consensus, actions) ->
        let consensus, additional_actions =
          incoming_block ~current ~block consensus
        in
        let actions = actions @ additional_actions in
        (consensus, actions))
      next_blocks (consensus, [])
  in
  let actions =
    Consensus_trigger_timeout { level }
    :: Consensus_accepted_block { block }
    :: actions
  in
  (consensus, actions)

and incoming_block ~current ~block consensus =
  let (Consensus ({ identity; block_pool; _ } as consensus)) = consensus in
  let block_pool = Block_pool.append_block ~block block_pool in
  let consensus = Consensus { consensus with block_pool } in
  let consensus, actions = incoming_block_or_vote ~current ~block consensus in
  let actions =
    match is_signable ~current ~consensus block with
    | true ->
        (* TODO: this will emit the same signature twice, is that bad? *)
        let vote = Block.sign ~identity block in
        Consensus_broadcast_vote { vote } :: actions
    | false -> actions
  in
  (consensus, actions)

let incoming_vote ~current ~vote consensus =
  let (Consensus ({ validators; block_pool; _ } as consensus)) = consensus in
  let validator = Verified_signature.key_hash vote in
  let hash = Verified_signature.signed_hash vote in
  let hash = Block_hash.of_blake2b hash in
  match Validators.mem validator validators with
  | true -> (
      let block_pool = Block_pool.append_vote ~validator ~hash block_pool in
      let consensus = Consensus { consensus with block_pool } in
      match Block_pool.find_block ~hash block_pool with
      | Some block -> incoming_block_or_vote ~current ~block consensus
      | None -> (consensus, []))
  | false -> (Consensus consensus, [])
