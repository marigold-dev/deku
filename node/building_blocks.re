open Helpers;
open Protocol;

module Node = State;
let is_known = (state, block) =>
  String_map.mem(block.Block.hash, state.Node.pending_blocks);
let is_valid_signature = (~hash, ~signature) => {
  // TODO: check if it's made by a known validator, avoid noise
  let Networking.{key, signature} = signature;
  let.ok signed =
    Signed.verify(~key, ~signature, hash)
    |> Result.map_error(_ => `Invalid_signature);
  signed |> Multisig.of_signed |> Multisig.signatures |> List.nth(_, 0) |> ok;
};

let is_valid_block_height = (state, block) =>
  block.Block.block_height >= state.Node.protocol.block_height;
let is_valid_block = (state, block) => {
  // TODO: check if it's made by a known validator?
  let is_all_operations_properly_signed = _block =>
    // TODO: move this out from of_yojson
    true;
  let.assert () = (
    "new block has a lower block height",
    is_valid_block_height(state, block),
  );

  // TODO: should we just ignore this operations?
  let.assert () = (
    "some operation in the block is not properly signed",
    is_all_operations_properly_signed(block),
  );

  Ok();
};

let is_next = (state, block) => Protocol.is_next(state.Node.protocol, block);

let is_signed_enough = (state, ~hash) => {
  let.default () = false;
  let.some block_and_signature =
    String_map.find_opt(hash, state.Node.pending_blocks);
  let signatures = block_and_signature.signatures;
  let number_of_validators =
    Validators.validators(state.protocol.validators) |> List.length;
  let needed_signatures =
    Float.(to_int(ceil(of_int(number_of_validators) *. (2.0 /. 3.0))));
  // TODO: properly filter and check signatures
  Some(List.length(signatures) >= needed_signatures);
};

let find_block_in_pool = (state, ~hash) => {
  let.some block_and_signatures =
    String_map.find_opt(hash, state.Node.pending_blocks);
  block_and_signatures.block;
};
let is_known_block = (state, ~hash) =>
  Option.is_some(find_block_in_pool(state, ~hash));
let is_known_signature = (state, ~hash, ~signature) => {
  let.default () = false;
  let.some block_and_signatures =
    String_map.find_opt(hash, state.Node.pending_blocks);
  Some(List.mem(signature, block_and_signatures.signatures));
};

let is_signed_by_key = (state, ~key, ~hash) => {
  let.default () = false;
  let.some block_and_signature =
    String_map.find_opt(hash, state.Node.pending_blocks);
  let signatures = block_and_signature.signatures;
  Some(signatures |> List.exists(signature => signature.Multisig.key == key));
};

let is_signed_by_self = (state, ~hash) =>
  is_signed_by_key(state, ~key=state.Node.identity.t, ~hash);
let get_current_block_producer = state =>
  if (state.Node.last_applied_block_timestamp == 0.0) {
    None;
  } else {
    // TODO: this is clearly dumb
    let rec next_until = (validators, diff) =>
      diff < 10.0
        ? validators : next_until(Validators.next(validators), diff -. 10.0);
    let diff = state.Node.last_applied_block_timestamp -. Unix.time();
    let validators = next_until(state.protocol.validators, diff);
    Validators.current(validators);
  };

let is_current_producer = (state, ~key) => {
  let.default () = false;
  let.some current_producer = get_current_block_producer(state);
  Some(current_producer.address == key);
};

let sign = (~key, ~hash) =>
  Signed.sign(~key, hash)
  |> Multisig.of_signed
  |> Multisig.signatures
  |> List.nth(_, 0);

let produce_block = state =>
  Block.make(
    ~previous_hash=last_block_hash(state.Node.protocol),
    ~author=state.Node.identity.t,
    ~block_height=Int64.add(state.protocol.block_height, 1L),
    ~main_chain_ops=state.pending_main_ops,
    ~side_chain_ops=state.pending_side_ops,
  );

// mutations
let append = (state, update_state, ~hash, ~signature) => {
  let block_and_signature =
    switch (String_map.find_opt(hash, state.Node.pending_blocks)) {
    | Some(block_and_signatures) => block_and_signatures
    | None => {signatures: [], block: None}
    };
  let pending_blocks =
    String_map.add(
      hash,
      {
        ...block_and_signature,
        signatures: [signature, ...block_and_signature.signatures],
      },
      state.Node.pending_blocks,
    );
  update_state({...state, pending_blocks});
};

let add_block_to_pool = (state, update_state, block) => {
  let block_and_signatures =
    switch (String_map.find_opt(block.Block.hash, state.Node.pending_blocks)) {
    | Some(block_and_signatures) => block_and_signatures
    | None => {signatures: [], block: None}
    };
  if (Option.is_some(block_and_signatures.block)) {
    Error(`Block_already_in_the_pool);
  } else {
    let block_and_signatures = {...block_and_signatures, block: Some(block)};
    let state = {
      ...state,
      pending_blocks:
        String_map.add(
          block.hash,
          block_and_signatures,
          state.pending_blocks,
        ),
    };
    Ok(update_state(state));
  };
};

let apply_block = (state, update_state, block) => {
  let.ok protocol = apply_block(state.Node.protocol, block);
  Ok(update_state({...state, protocol}));
};

let clean = (state, update_state, block) => {
  // TODO: this is the dumbest piece of code that I could write
  let main_is_in_block = side_op =>
    block.Block.main_chain_ops |> List.exists(op => side_op == op);
  let side_is_in_block = side_op =>
    block.side_chain_ops |> List.exists(op => side_op == op);

  let pending_main_ops =
    state.Node.pending_main_ops |> List.filter(main_is_in_block);
  let pending_side_ops =
    state.pending_side_ops |> List.filter(side_is_in_block);
  // TODO: clean old blocks and old signatures
  update_state({
    ...state,
    pending_main_ops,
    pending_side_ops,
    last_applied_block_timestamp: Unix.time(),
  });
};

// networking functions

// TODO: maybe send to some specific nodes so that they should broadcast it?
// maybe have a random chance of re-broadcasting it to the network
let broadcast_signature = (state, ~hash, ~signature) =>
  Lwt.async(() =>
    Networking.broadcast_signature(
      state,
      {
        hash,
        signature:
          Multisig.{key: signature.key, signature: signature.signature},
      },
    )
  );
let broadcast_block_and_signature = (state, ~block, ~signature) =>
  Lwt.async(() => {
    let.await () = Lwt_unix.sleep(1.0);
    Networking.broadcast_block_and_signature(
      state,
      {
        block,
        signature:
          Multisig.{key: signature.key, signature: signature.signature},
      },
    );
  });
