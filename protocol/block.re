open Operation;
open Helpers;
open Crypto;

[@deriving yojson]
type t = {
  // TODO: validate this hash on yojson
  // TODO: what if block hash was a merkle tree of previous_hash + state_root_hash + block_data
  // block header
  // sha256(state_root_hash + payload_hash)
  hash: BLAKE2B.t,
  // TODO: is it okay to payload_hash to appears on both sides?
  // sha256(json of all fields including payload hash)
  payload_hash: BLAKE2B.t,
  state_root_hash: BLAKE2B.t,
  handles_hash: BLAKE2B.t,
  validators_hash: BLAKE2B.t,
  previous_hash: BLAKE2B.t,
  // block data
  author: Address.t,
  // TODO: do we need a block_height? What is the tradeoffs?
  // TODO: maybe it should be only for internal pagination and stuff like this
  block_height: int64,
  main_chain_ops: list(Main_chain.t),
  side_chain_ops: list(Side_chain.t),
};

let (hash, verify) = {
  /* TODO: this is bad name, it exists like this to prevent
     duplicating all this name parameters */

  let apply =
      (
        f,
        ~state_root_hash,
        ~handles_hash,
        ~validators_hash,
        ~previous_hash,
        ~author,
        ~block_height,
        ~main_chain_ops,
        ~side_chain_ops,
      ) => {
    let to_yojson = [%to_yojson:
      (
        BLAKE2B.t,
        BLAKE2B.t,
        BLAKE2B.t,
        BLAKE2B.t,
        // block data
        Address.t,
        int64,
        list(Main_chain.t),
        list(Side_chain.t),
      )
    ];
    let json =
      to_yojson((
        state_root_hash,
        handles_hash,
        validators_hash,
        previous_hash,
        author,
        block_height,
        main_chain_ops,
        side_chain_ops,
      ));
    let payload = Yojson.Safe.to_string(json);
    let block_payload_hash = BLAKE2B.hash(payload);
    // TODO: is it okay to have this string concatened here?
    // TODO: maybe should also be previous?

    let hash =
      Tezos.Deku.Consensus.hash_block(
        ~block_height,
        ~block_payload_hash,
        ~state_root_hash,
        ~handles_hash,
        ~validators_hash,
      );
    /*
       double hash because tezos always uses blake2b on CHECK_SIGNATURE
       https://gitlab.com/tezos/tezos/-/blob/cf95d1507a13efb3ff4fb247aabb44efc0082fa7/src/lib_crypto/ed25519.ml#L338
     */
    let hash = BLAKE2B.hash(BLAKE2B.to_raw_string(hash));
    f((hash, block_payload_hash));
  };
  let hash = apply(Fun.id);
  let verify = (~hash as expected_hash) =>
    apply(((hash, _payload_hash))
      // TODO: this logic is duplicated from BLAKE2B.verify
      => hash == expected_hash);
  (hash, verify);
};
// if needed we can export this, it's safe
let make =
    (
      ~state_root_hash,
      ~handles_hash,
      ~validators_hash,
      ~previous_hash,
      ~author,
      ~block_height,
      ~main_chain_ops,
      ~side_chain_ops,
    ) => {
  let (hash, payload_hash) =
    hash(
      ~state_root_hash,
      ~handles_hash,
      ~validators_hash,
      ~previous_hash,
      ~author,
      ~block_height,
      ~main_chain_ops,
      ~side_chain_ops,
    );
  {
    hash,
    payload_hash,
    previous_hash,

    state_root_hash,
    handles_hash,
    validators_hash,
    author,
    block_height,
    main_chain_ops,
    side_chain_ops,
  };
};

let of_yojson = json => {
  let.ok block = of_yojson(json);
  let.ok () =
    verify(
      ~hash=block.hash,
      ~state_root_hash=block.state_root_hash,
      ~handles_hash=block.handles_hash,
      ~validators_hash=block.validators_hash,
      ~previous_hash=block.previous_hash,
      ~author=block.author,
      ~block_height=block.block_height,
      ~main_chain_ops=block.main_chain_ops,
      ~side_chain_ops=block.side_chain_ops,
    )
      ? Ok() : Error("Invalid hash");
  Ok(block);
};

let compare = (a, b) => BLAKE2B.compare(a.hash, b.hash);

let genesis =
  make(
    ~previous_hash=BLAKE2B.hash("tuturu"),
    ~state_root_hash=BLAKE2B.hash("mayuushi"),
    ~handles_hash=BLAKE2B.hash("desu"),
    ~validators_hash=Validators.hash(Validators.empty),
    ~block_height=0L,
    ~main_chain_ops=[],
    ~side_chain_ops=[],
    ~author=Address.of_wallet(Wallet.genesis_wallet),
  );

// TODO: move this to a global module
let state_root_hash_epoch = 60.0;
/** to prevent changing the validator just because of network jittering
    this introduce a delay between can receive a block with new state
    root hash and can produce that block

    1s choosen here but any reasonable time will make it */
let avoid_jitter = 1.0;
let _can_update_state_root_hash = state =>
  Unix.time() -. state.State.last_state_root_update >= state_root_hash_epoch;
let can_produce_with_new_state_root_hash = state =>
  Unix.time()
  -. state.State.last_state_root_update
  -. avoid_jitter >= state_root_hash_epoch;
let produce = (~state) => {
  let update_state_hashes = can_produce_with_new_state_root_hash(state);
  make(
    ~previous_hash=state.State.last_block_hash,
    ~state_root_hash=
      update_state_hashes ? fst(State.hash(state)) : state.state_root_hash,
    ~handles_hash=Ledger.handles_root_hash(state.ledger),
    ~validators_hash=Validators.hash(state.validators),
    ~block_height=Int64.add(state.block_height, 1L),
  );
};

// TODO: this shouldn't be an open
open Protocol_signature.Make({
       type nonrec t = t;
       let hash = t => t.hash;
     });

let sign = (~key, t) => sign(~key, t).signature;
let verify = (~signature, t) => verify(~signature, t);
