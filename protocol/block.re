open Operation;
open Helpers;

[@deriving yojson]
type t = {
  // TODO: validate this hash on yojson
  // TODO: what if block hash was a merkle tree of previous_hash + state_root_hash + block_data
  // block header
  hash: SHA256.hash,
  previous_hash: SHA256.hash,
  state_root_hash: SHA256.hash,
  // block data
  author: Address.t,
  // TODO: do we need a block_height? What is the tradeoffs?
  // TODO: maybe it should be only for internal pagination and stuff like this
  block_height: int64,
  main_chain_ops: list(Main_chain.t),
  side_chain_ops: list(Side_chain.Self_signed.t),
};
// TODO: this shouldn't be an open
open Signature.Make({
       type nonrec t = t;
       let hash = t => t.hash;
     });
module Signature = Signature;

// if needed we can export this, it's safe
let make =
    (
      ~previous_hash,
      ~state_root_hash,
      ~author,
      ~block_height,
      ~main_chain_ops,
      ~side_chain_ops,
    ) => {
  let SHA256.Magic.{hash, _} =
    SHA256.Magic.hash((
      previous_hash,
      state_root_hash,
      author,
      block_height,
      main_chain_ops,
      side_chain_ops,
    ));
  {
    hash,
    previous_hash,

    state_root_hash,
    author,
    block_height,
    main_chain_ops,
    side_chain_ops,
  };
};

let of_yojson = json => {
  let.ok {
    hash,
    previous_hash,
    state_root_hash,
    author,
    block_height,
    main_chain_ops,
    side_chain_ops,
  } =
    of_yojson(json);
  let.ok {hash, _} =
    SHA256.Magic.verify(
      ~hash,
      (
        previous_hash,
        state_root_hash,
        author,
        block_height,
        main_chain_ops,
        side_chain_ops,
      ),
    );
  Ok({
    hash,
    previous_hash,
    state_root_hash,
    author,
    block_height,
    main_chain_ops,
    side_chain_ops,
  });
};

let compare = (a, b) => SHA256.compare_hash(a.hash, b.hash);

let genesis =
  make(
    ~previous_hash=SHA256.Magic.hash("tuturu").hash,
    ~state_root_hash=SHA256.Magic.hash("mayuushi-desu").hash,
    ~block_height=0L,
    ~main_chain_ops=[],
    ~side_chain_ops=[],
    ~author=Address.genesis_address,
  );

// TODO: move this to a global module
let state_root_hash_epoch = 6.0;
/** to prevent changing the validator just because of network jittering
    this introduce a delay between can receive a block with new state
    root hash and can produce that block

    10s choosen here but any reasonable time will make it */
let avoid_jitter = 1.0;
let _can_update_state_root_hash = state =>
  Unix.time() -. state.State.last_state_root_update >= state_root_hash_epoch;
let can_produce_with_new_state_root_hash = state =>
  Unix.time()
  -. state.State.last_state_root_update
  -. avoid_jitter >= state_root_hash_epoch;
let produce = (~state) =>
  make(
    ~previous_hash=state.State.last_block_hash,
    ~state_root_hash=
      can_produce_with_new_state_root_hash(state)
        ? state.pending_state_root_hash : state.state_root_hash,
    ~block_height=Int64.add(state.block_height, 1L),
  );
let sign = (~key, t) => sign(~key, t).signature;
let verify = (~signature, t) => verify(~signature, t);
