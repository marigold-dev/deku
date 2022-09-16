open Deku_stdlib
open Deku_concepts
open Block

type action =
  (* protocol *)
  | Consensus_accepted_block of { level : Level.t; payload : string list }
  (* timer *)
  | Consensus_trigger_timeout of { level : Level.t }
  (* network *)
  | Consensus_broadcast_signature of { signature : Verified_signature.t }

type consensus =
  | Consensus of {
      block_pool : Block_pool.t;
      signer : Signer.t;
      state : State.t;
    }

and t = consensus

let make ~identity ~validators =
  let block_pool = Block_pool.empty in
  let signer = Signer.make ~identity in
  let state = State.genesis ~validators in
  Consensus { block_pool; signer; state }

let rec incoming_block_or_signature ~current ~block consensus =
  match
    let (Consensus { block_pool; signer = _; state }) = consensus in
    Judger.is_accepted ~state ~block_pool block
  with
  | true -> with_accepted_block ~current ~block consensus
  | false -> (consensus, [])

and with_accepted_block ~current ~block consensus =
  let (Consensus { block_pool; signer; state }) = consensus in
  let (Block { hash; level; payload; _ }) = block in
  let () =
    let level = Level.to_n level in
    let level = N.to_z level in
    Format.eprintf "%a\n%!" Z.pp_print level
  in
  let state = State.apply_block ~current ~block state in
  let block_pool = Block_pool.clean ~current block_pool in
  let consensus = Consensus { block_pool; signer; state } in

  let blocks = Block_pool.find_next_blocks ~block_previous:hash block_pool in
  let consensus, actions =
    Block.Set.fold
      (fun block (consensus, actions) ->
        let consensus, additional_actions =
          incoming_block ~current ~block consensus
        in
        let actions = actions @ additional_actions in
        (consensus, actions))
      blocks (consensus, [])
  in
  let actions =
    Consensus_trigger_timeout { level }
    :: Consensus_accepted_block { level; payload }
    :: actions
  in
  (consensus, actions)

and incoming_block ~current ~block consensus =
  let (Consensus { block_pool; signer; state }) = consensus in
  let block_pool = Block_pool.append_block ~current block block_pool in
  let consensus = Consensus { block_pool; signer; state } in

  let consensus, actions =
    incoming_block_or_signature ~current ~block consensus
  in
  let actions =
    match Judger.is_signable ~current ~state block with
    | true -> (
        let signature =
          (* TODO: this will emit the same signature twice, isn't that bad? *)
          match Judger.is_signable ~current ~state block with
          | true -> Some (Signer.sign ~block signer)
          | false -> None
        in
        match signature with
        | Some signature ->
            Consensus_broadcast_signature { signature } :: actions
        | None -> actions)
    | false -> actions
  in
  (consensus, actions)

let incoming_signature ~current ~signature consensus =
  let (Consensus { block_pool; signer; state }) = consensus in
  let block_pool = Block_pool.append_signature ~current signature block_pool in
  let consensus = Consensus { block_pool; signer; state } in

  let block_hash = Verified_signature.signed_hash signature in
  let block_hash = Block_hash.of_blake2b block_hash in
  match Block_pool.find_block ~block_hash block_pool with
  | Some block -> incoming_block_or_signature ~current ~block consensus
  | None -> (consensus, [])
