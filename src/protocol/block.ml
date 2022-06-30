open Helpers
open Crypto
open Core_deku

type t = {
  hash : BLAKE2B.t;
  payload_hash : BLAKE2B.t;
  state_root_hash : BLAKE2B.t;
  withdrawal_handles_hash : BLAKE2B.t;
  validators_hash : BLAKE2B.t;
  previous_hash : BLAKE2B.t;
  author : Key_hash.t;
  block_height : int64;
  consensus_operations : Protocol_operation.Consensus.t list;
  tezos_operations : Protocol_operation.Core_tezos.t list;
  user_operations : string;
}
[@@deriving yojson]

type user_operations = Protocol_operation.Core_user.t list [@@deriving yojson]

let serialize_user_operations user_operations =
  let json = user_operations_to_yojson user_operations in
  Yojson.Safe.to_string json

let parse_user_operations t =
  let%ok json =
    try Ok (Yojson.Safe.from_string t.user_operations) with
    | _exn -> Error "invalid json" in
  user_operations_of_yojson json

let parse_user_operations t =
  (* invalid encode means noop *)
  match parse_user_operations t with
  | Ok user_operations -> user_operations
  | Error _ ->
    (* TODO: maybe log this *)
    []

let hash, verify =
  let apply f ~state_root_hash ~withdrawal_handles_hash ~validators_hash
      ~previous_hash ~author ~block_height ~consensus_operations
      ~tezos_operations ~user_operations =
    let to_yojson =
      [%to_yojson:
        BLAKE2B.t
        * BLAKE2B.t
        * BLAKE2B.t
        * BLAKE2B.t
        * Key_hash.t
        * int64
        * Protocol_operation.Consensus.t list
        * Protocol_operation.Core_tezos.t list
        * string] in
    let json =
      to_yojson
        ( state_root_hash,
          withdrawal_handles_hash,
          validators_hash,
          previous_hash,
          author,
          block_height,
          consensus_operations,
          tezos_operations,
          user_operations ) in
    let payload = Yojson.Safe.to_string json in
    let block_payload_hash = BLAKE2B.hash payload in
    let hash =
      Tezos.Deku.Consensus.hash_block ~block_height ~block_payload_hash
        ~state_root_hash ~withdrawal_handles_hash ~validators_hash in
    let hash = BLAKE2B.hash (BLAKE2B.to_raw_string hash) in
    f (hash, block_payload_hash) in
  let hash = apply Fun.id in
  let verify ~hash:expected_hash ~payload_hash:expected_payload_hash =
    apply (fun (hash, payload_hash) ->
        BLAKE2B.equal hash expected_hash
        && BLAKE2B.equal payload_hash expected_payload_hash) in
  (hash, verify)

let of_yojson json =
  let%ok block = of_yojson json in
  let%ok () =
    match
      let {
        hash;
        payload_hash;
        state_root_hash;
        withdrawal_handles_hash;
        validators_hash;
        previous_hash;
        author;
        block_height;
        consensus_operations;
        tezos_operations;
        user_operations;
      } =
        block in
      verify ~hash ~payload_hash ~state_root_hash ~withdrawal_handles_hash
        ~validators_hash ~previous_hash ~author ~block_height
        ~consensus_operations ~tezos_operations ~user_operations
    with
    | true -> Ok ()
    | false -> Error "Invalid hash" in
  Ok block

let compare a b = BLAKE2B.compare a.hash b.hash

let make ~state_root_hash ~withdrawal_handles_hash ~validators_hash
    ~previous_hash ~author ~block_height ~consensus_operations ~tezos_operations
    ~user_operations =
  let user_operations = serialize_user_operations user_operations in
  let hash, payload_hash =
    hash ~state_root_hash ~withdrawal_handles_hash ~validators_hash
      ~previous_hash ~author ~block_height ~consensus_operations
      ~tezos_operations ~user_operations in
  Log.info "bar";
  {
    hash;
    payload_hash;
    previous_hash;
    state_root_hash;
    withdrawal_handles_hash;
    validators_hash;
    author;
    block_height;
    consensus_operations;
    tezos_operations;
    user_operations;
  }

let genesis =
  make ~previous_hash:(BLAKE2B.hash "tuturu")
    ~state_root_hash:(BLAKE2B.hash "mayuushi")
    ~withdrawal_handles_hash:(BLAKE2B.hash "desu")
    ~validators_hash:(Validators.hash Validators.empty)
    ~block_height:0L ~consensus_operations:[] ~tezos_operations:[]
    ~user_operations:[]
    ~author:(Key_hash.of_key Wallet.genesis_wallet)

let produce ~state ~next_state_root_hash =
  let next_state_root_hash =
    Option.value ~default:state.Protocol_state.state_root_hash
      next_state_root_hash in
  Log.info "waaaaat";
  make ~previous_hash:state.Protocol_state.last_block_hash
    ~state_root_hash:next_state_root_hash
    ~withdrawal_handles_hash:
      (Core_deku.State.ledger state.core_state
      |> Ledger.withdrawal_handles_root_hash)
    ~validators_hash:(Validators.hash state.validators)
    ~block_height:(Int64.add state.block_height 1L)

open Protocol_signature.Make (struct
  type nonrec t = t

  let hash t = t.hash
end)

let sign ~key t = (sign ~key t).signature

let pp fmt t =
  let hash_str = BLAKE2B.to_string t.hash in
  let short_hash = String.sub hash_str 0 8 in
  Format.fprintf fmt "Block %s %Ld" short_hash t.block_height
