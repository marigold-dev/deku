open Deku_crypto
open Deku_consensus
open Deku_concepts

let (Block.Block { level = genesis_level; hash = genesis_hash; _ }) =
  Genesis.block

(* FIXME: Copied these hard-coded hashes *)
let state_root_hash = BLAKE2b.hash "FIXME: we need to add the state root"
let withdrawal_handles_hash = BLAKE2b.hash "tuturu"

let identities =
  [
    "edsk4UWkJqpZrAm26qvJE8uY9ZFGFqQiFuBcDyEPASXeHxuD68WvvF";
    "edsk2mbL2Z7bAmRnuYbmsRe8Yu9rgAq1h993SDxoZncmqyMHDECyBa";
    "edsk3dx8ZfcaBXsuLsk8fawS1qxjHbZtEoEdpAwxhsjmYTQhoEUxFk";
    "edsk3MwFfcGp5FsZgrX8FGiBiDutX2kfAuPzU6VdZpKYLyDRVPb879";
  ]
  |> List.map (fun secret ->
         let secret = Ed25519.Secret.of_b58 secret |> Option.get in
         let secret = Secret.Ed25519 secret in
         Identity.make secret)

let some_block =
  let producer_identity = List.nth identities 0 in
  let level = Level.next genesis_level in
  Block.produce ~identity:producer_identity ~level ~previous:genesis_hash
    ~payload:[] ~withdrawal_handles_hash ~tezos_operations:[]

let some_block_keys_and_signatures =
  identities
  |> List.map (fun identity ->
         let signature =
           Block.sign ~identity some_block |> Verified_signature.signature
         in
         Some (Identity.key identity, signature))

let validators = List.map Identity.key_hash identities
