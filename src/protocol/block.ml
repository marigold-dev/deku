open Helpers
open Crypto
open Core
type t = {
  hash : BLAKE2B.t;
  payload_hash : BLAKE2B.t;
  state_root_hash : BLAKE2B.t;
  withdrawal_handles_hash : BLAKE2B.t;
  validators_hash : BLAKE2B.t;
  previous_hash : BLAKE2B.t;
  author : Key_hash.t;
  block_height : int64;
  operations : Protocol_operation.t list;
}
[@@deriving yojson]
let hash, verify =
  let apply f ~state_root_hash ~withdrawal_handles_hash ~validators_hash
      ~previous_hash ~author ~block_height ~operations =
    let to_yojson =
      [%to_yojson:
        BLAKE2B.t
        * BLAKE2B.t
        * BLAKE2B.t
        * BLAKE2B.t
        * Key_hash.t
        * int64
        * Protocol_operation.t list] in
    let json =
      to_yojson
        ( state_root_hash,
          withdrawal_handles_hash,
          validators_hash,
          previous_hash,
          author,
          block_height,
          operations ) in
    let payload = Yojson.Safe.to_string json in
    let block_payload_hash = BLAKE2B.hash payload in
    let hash =
      Tezos.Deku.Consensus.hash_block ~block_height ~block_payload_hash
        ~state_root_hash ~withdrawal_handles_hash ~validators_hash in
    let hash = BLAKE2B.hash (BLAKE2B.to_raw_string hash) in
    f (hash, block_payload_hash) in
  let hash = apply Fun.id in
  let verify ~hash:expected_hash =
    apply (fun (hash, _payload_hash) -> hash = expected_hash) in
  (hash, verify)
let make ~state_root_hash ~withdrawal_handles_hash ~validators_hash
    ~previous_hash ~author ~block_height ~operations =
  let hash, payload_hash =
    hash ~state_root_hash ~withdrawal_handles_hash ~validators_hash
      ~previous_hash ~author ~block_height ~operations in
  {
    hash;
    payload_hash;
    previous_hash;
    state_root_hash;
    withdrawal_handles_hash;
    validators_hash;
    author;
    block_height;
    operations;
  }
let of_yojson json =
  let%ok block = of_yojson json in
  let%ok () =
    match
      verify ~hash:block.hash ~state_root_hash:block.state_root_hash
        ~withdrawal_handles_hash:block.withdrawal_handles_hash
        ~validators_hash:block.validators_hash
        ~previous_hash:block.previous_hash ~author:block.author
        ~block_height:block.block_height ~operations:block.operations
    with
    | true -> Ok ()
    | false -> Error "Invalid hash" in
  Ok block
let compare a b = BLAKE2B.compare a.hash b.hash
let genesis =
  make ~previous_hash:(BLAKE2B.hash "tuturu")
    ~state_root_hash:(BLAKE2B.hash "mayuushi")
    ~withdrawal_handles_hash:(BLAKE2B.hash "desu")
    ~validators_hash:(Validators.hash Validators.empty)
    ~block_height:0L ~operations:[]
    ~author:(Key_hash.of_key Wallet.genesis_wallet)
let produce ~state ~next_state_root_hash =
  let next_state_root_hash =
    Option.value ~default:state.Protocol_state.state_root_hash
      next_state_root_hash in
  make ~previous_hash:state.Protocol_state.last_block_hash
    ~state_root_hash:next_state_root_hash
    ~withdrawal_handles_hash:
      (Core.State.ledger state.core_state |> Ledger.withdrawal_handles_root_hash)
    ~validators_hash:(Validators.hash state.validators)
    ~block_height:(Int64.add state.block_height 1L)
open Protocol_signature.Make (struct
  type nonrec t = t
  let hash t = t.hash
end)
let sign ~key t = (sign ~key t).signature
let verify ~signature t = verify ~signature t
