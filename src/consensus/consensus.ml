open Deku_stdlib
open Deku_concepts
open Block

type external_effect =
  (* protocol *)
  | Accepted_block of { level : Level.t; payload : string list }
  (* timer *)
  | Trigger_timeout
  (* network *)
  | Broadcast_signature of { signature : Verified_signature.t }

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

let incoming_block_or_signature ~current ~block consensus =
  let (Consensus { block_pool; signer; state }) = consensus in
  let state, effects =
    match Judger.is_accepted ~state ~block_pool block with
    | true ->
        let (Block { level; payload; _ }) = block in
        let () =
          let level = Level.to_n level in
          let level = N.to_z level in
          Format.eprintf "%a\n%!" Z.pp_print level
        in
        let state = State.apply_block ~current ~block state in
        (state, [ Trigger_timeout; Accepted_block { level; payload } ])
    | false -> (state, [])
  in

  let consensus = Consensus { block_pool; signer; state } in
  (consensus, effects)

let incoming_block ~current ~block consensus =
  let (Consensus { block_pool; signer; state }) = consensus in
  let block_pool = Block_pool.append_block block block_pool in
  let consensus = Consensus { block_pool; signer; state } in

  let consensus, effects =
    incoming_block_or_signature ~current ~block consensus
  in
  let effects =
    match Judger.is_signable ~current ~state block with
    | true -> (
        let signature =
          (* TODO: this will emit the same signature twice, isn't that bad? *)
          match Judger.is_signable ~current ~state block with
          | true -> Some (Signer.sign ~block signer)
          | false -> None
        in
        match signature with
        | Some signature -> Broadcast_signature { signature } :: effects
        | None -> effects)
    | false -> effects
  in
  (consensus, effects)

let incoming_signature ~current ~signature consensus =
  let (Consensus { block_pool; signer; state }) = consensus in
  let block_pool = Block_pool.append_signature signature block_pool in
  let consensus = Consensus { block_pool; signer; state } in

  let block_hash = Verified_signature.signed_hash signature in
  let block_hash = Block_hash.of_blake2b block_hash in
  match Block_pool.find_block ~block_hash block_pool with
  | Some block -> incoming_block_or_signature ~current ~block consensus
  | None -> (consensus, [])
