open Deku_stdlib
open Deku_concepts
open Deku_protocol
open Deku_consensus
open Deku_crypto

type chain =
  | Chain of {
      protocol : Protocol.t;
      consensus : Consensus.t;
      verifier : Verifier.t;
      signer : Signer.t;
      producer : Producer.t;
    }

type t = chain

type external_effect =
  | Reset_timeout
  | Broadcast_block of Block.t
  | Broadcast_signature of Verified_signature.t
  | Save_block of Block.t
  | Commit_state_hash of {
      current_level : Level.t;
      payload_hash : BLAKE2b.t;
      state_root_hash : BLAKE2b.t;
      withdrawal_handles_hash : Deku_protocol.Ledger.Withdrawal_handle.hash;
      signatures : (Key.t * Signature.t) option list;
      validators : Key_hash.t list;
    }

let make ~identity ~bootstrap_key ~validators =
  let validators = Validators.of_key_hash_list validators in
  let protocol = Protocol.initial in
  let consensus = Consensus.make ~validators ~bootstrap_key in
  let verifier = Verifier.empty in
  let signer = Signer.make ~identity in
  let producer = Producer.make ~identity in
  Chain { protocol; consensus; verifier; signer; producer }

let commit current_level block verifier validators =
  let Block.(
        Block { payload_hash; hash = block_hash; withdrawal_handles_hash; _ }) =
    block
  in
  let state_root_hash =
    Deku_crypto.BLAKE2b.hash "FIXME: we need to add the state root"
  in
  let signatures = Verifier.find_signatures ~block_hash verifier in
  let signatures =
    List.map
      (fun key_hash ->
        let%some verified = Key_hash.Map.find_opt key_hash signatures in
        Some
          ( Verified_signature.key verified,
            Verified_signature.signature verified ))
      validators
  in
  Commit_state_hash
    {
      current_level;
      payload_hash;
      state_root_hash;
      withdrawal_handles_hash;
      signatures;
      validators;
    }

let apply_block ~pool ~current ~block chain =
  let (Block.Block { level; payload; tezos_operations; _ }) = block in
  let () = Format.eprintf "%a\n%!" N.pp (Level.to_n level) in
  let (Chain { protocol; consensus; verifier; signer; producer }) = chain in
  let consensus = Consensus.apply_block ~current ~block consensus in
  let protocol, receipts =
    Protocol.apply
      ~parallel:(fun f l -> Parallel.filter_map_p pool f l)
      ~current_level:level ~payload protocol ~tezos_operations
  in
  let producer = Producer.clean ~receipts ~tezos_operations producer in
  let withdrawal_handles_hash = Protocol.withdrawal_handles_hash protocol in
  (* FIXME: need a time-based procedure for this, not block-based *)
  (* FIXME: rediscuss the need to commit the previous block instead *)
  (* FIXME: validators have to watch when commit did not happen *)
  let (Consensus { current_level; validators; _ }) = consensus in
  let validators = Validators.to_key_hash_list validators in
  let level = Level.to_n current_level |> N.to_z |> Z.to_int in
  let effects =
    match
      Producer.try_to_produce
        ~parallel_map:(fun f l -> Parallel.map_p pool f l)
        ~current ~consensus ~withdrawal_handles_hash producer
    with
    (* FIXME: weird place to commit *)
    | Some new_block when level mod 5 = 0 ->
        let commit_effect = commit current_level block verifier validators in
        [ Broadcast_block new_block; commit_effect ]
    | Some block -> [ Broadcast_block block ]
    | None -> []
  in
  let effects = Save_block block :: Reset_timeout :: effects in
  (Chain { protocol; consensus; verifier; signer; producer }, effects)

let incoming_block ~pool ~current ~block chain =
  let (Chain { protocol; consensus; verifier; signer; producer }) = chain in
  let Verifier.{ apply; verifier } =
    Verifier.incoming_block ~consensus ~block verifier
  in
  let chain = Chain { protocol; consensus; verifier; signer; producer } in

  let effects =
    match Signer.try_to_sign ~current ~consensus ~block signer with
    | Some signature -> [ Broadcast_signature signature ]
    | None -> []
  in
  match apply with
  | Some block ->
      let chain, additional_effects = apply_block ~pool ~current ~block chain in
      (chain, effects @ additional_effects)
  | None -> (chain, effects)

let incoming_signature ~pool ~current ~signature chain =
  let (Chain { protocol; consensus; verifier; signer; producer }) = chain in
  let Verifier.{ apply; verifier } =
    Verifier.incoming_signature ~consensus ~signature verifier
  in
  let chain = Chain { protocol; consensus; verifier; signer; producer } in

  match apply with
  | Some block -> apply_block ~pool ~current ~block chain
  | None -> (chain, [])

let incoming_timeout ~pool ~current node =
  let () = Format.eprintf "timeout\n%!" in
  let (Chain { protocol; consensus; verifier; signer; producer }) = node in
  let withdrawal_handles_hash = Protocol.withdrawal_handles_hash protocol in
  let effects =
    (* TODO: do not like duplicating this lambda everywhere. *)
    match
      Producer.try_to_produce
        ~parallel_map:(fun f l -> Parallel.map_p pool f l)
        ~current ~consensus producer ~withdrawal_handles_hash
    with
    | Some block -> [ Broadcast_block block ]
    | None -> []
  in
  (Chain { protocol; consensus; verifier; signer; producer }, effects)

let incoming_operation ~operation node =
  let (Chain { protocol; consensus; verifier; signer; producer }) = node in
  let producer = Producer.incoming_operation ~operation producer in
  Chain { protocol; consensus; verifier; signer; producer }

let incoming_bootstrap_signal ~pool ~bootstrap_signal ~current node =
  let (Chain { protocol; consensus; verifier; signer; producer }) = node in
  let consensus =
    match
      Consensus.apply_bootstrap_signal ~bootstrap_signal ~current consensus
    with
    | Some consensus -> consensus
    | None -> consensus
  in
  let effects =
    let withdrawal_handles_hash = Protocol.withdrawal_handles_hash protocol in
    match
      Producer.try_to_produce
        ~parallel_map:(fun f l -> Parallel.map_p pool f l)
        ~current ~consensus ~withdrawal_handles_hash producer
    with
    | Some block -> [ Broadcast_block block ]
    | None -> []
  in
  (Chain { protocol; consensus; verifier; signer; producer }, effects)

let incoming_tezos_operation ~tezos_operation chain =
  let (Chain { protocol; consensus; verifier; signer; producer }) = chain in
  let producer = Producer.incoming_tezos_operation ~tezos_operation producer in
  (Chain { protocol; consensus; verifier; signer; producer }, [])
