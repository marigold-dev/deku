open Deku_crypto
open Deku_concepts
open Deku_consensus
open Deku_chain

let get_current () = Timestamp.of_float (Unix.gettimeofday ())
(* TODO: Abstract this generation process *)
let secret1 = Ed25519.Secret.generate ()
let secret1 = Secret.Ed25519 secret1
let identity1 = Identity.make secret1
let secret2 = Ed25519.Secret.generate ()
let secret2 = Secret.Ed25519 secret2
let identity2 = Identity.make secret2
let secret3 = Ed25519.Secret.generate ()
let secret3 = Secret.Ed25519 secret3
let identity3 = Identity.make secret3
let secret4 = Ed25519.Secret.generate ()
let secret4 = Secret.Ed25519 secret4
let identity4 = Identity.make secret4

let validators =
  let key1 = Key.of_secret secret1 in
  let key2 = Key.of_secret secret2 in
  let key3 = Key.of_secret secret3 in
  let key4 = Key.of_secret secret4 in
  let key_hash1 = Key_hash.of_key key1 in
  let key_hash2 = Key_hash.of_key key2 in
  let key_hash3 = Key_hash.of_key key3 in
  let key_hash4 = Key_hash.of_key key4 in
  [ key_hash1; key_hash2; key_hash3; key_hash4 ]

(* We have the sender, receiver, and backup chains.
   Now we create some blocks, change the chain states,
   and mess with the perception of the receiver chain
*)
let sender_chain = Chain.make ~identity:identity1 ~validators
let receiver_chain = Chain.make ~identity:identity2 ~validators
let backup_chain1 = Chain.make ~identity:identity3 ~validators
let backup_chain2 = Chain.make ~identity:identity4 ~validators
let chains = [ sender_chain; receiver_chain; backup_chain1; backup_chain2 ]

let first_block =
  let leader = List.nth chains 0 in
  let (Chain.Chain { consensus; _ }) = leader in
  let (Consensus { current_block; identity; _ }) = consensus in
  let (Block { hash = current_block; level = current_level; _ }) =
    current_block
  in
  let level = Level.next current_level in
  let previous = current_block in
  let operations = [] in
  Block.produce ~identity ~level ~previous ~operations

(* Chains after we've applied votes from the first block
   Does not drive code, for initialization only *)
let chains_after_first_block =
  (* Chains and actions after incoming blocks *)
  let chains_actions =
    List.map
      (fun chain ->
        Chain.incoming_block ~current:(get_current ()) ~block:first_block chain)
      chains
  in

  (* Collect votes to first block from all chains *)
  let votes =
    List.map
      (fun chain ->
        let (Chain.Chain { consensus; _ }) = chain in
        let (Consensus { identity; _ }) = consensus in
        Block.sign ~identity first_block)
      chains
  in
  (* Apply votes to chain, actions *)
  let apply_votes chain =
    List.fold_left
      (fun (chain, initial_actions) vote ->
        let chain, actions =
          Chain.incoming_vote ~current:(get_current ()) ~vote chain
        in

        (chain, initial_actions @ actions))
      chain votes
  in
  let chains_actions = List.map apply_votes chains_actions in
  List.iter
    (fun (_, actions) -> List.iter Chain.pp_action actions)
    chains_actions;
  chains_actions

module Map = Key_hash.Map

let chains_actions_map =
  let module Map = Key_hash.Map in
  List.fold_left2
    (fun map validator chain_actions -> Map.add validator chain_actions map)
    Map.empty validators chains_after_first_block
