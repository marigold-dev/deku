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

let check_block_and_signature_is_self_signed = (state, block_and_signature) => {
  let signatures = block_and_signature.Node.signatures;
  let number_of_validators =
    Validators.validators(state.Node.protocol.validators) |> List.length;
  let needed_signatures =
    Float.(to_int(ceil(of_int(number_of_validators) *. (2.0 /. 3.0))));
  // TODO: properly filter and check signatures
  List.length(signatures) >= needed_signatures;
};
let rec check_block_and_signature_is_enough = (state, block_and_signature) => {
  // TODO: this will blownup if there is a cycle
  let.default () = false;
  if (check_block_and_signature_is_self_signed(state, block_and_signature)) {
    Some(true);
  } else {
    let.some pending_parent_blocks =
      String_map.find_opt(
        block_and_signature.hash,
        state.Node.pending_blocks_by_previous,
      );
    // TODO: didn't like this code
    Some(
      List.exists(
        check_block_and_signature_is_enough(state),
        pending_parent_blocks,
      ),
    );
  };
};

let is_self_signed_block = (state, ~hash) => {
  let.default () = false;
  let.some block_and_signature =
    String_map.find_opt(hash, state.Node.pending_blocks);
  Some(check_block_and_signature_is_self_signed(state, block_and_signature));
};
let is_signed_enough = (state, ~hash) => {
  let.default () = false;
  let.some block_and_signature =
    String_map.find_opt(hash, state.Node.pending_blocks);
  Some(check_block_and_signature_is_enough(state, block_and_signature));
};

// TODO: bad naming
let is_latest = (state, block) =>
  state.Node.last_signed_block == Some(block);
let find_next_block_to_apply = (state, block) => {
  let.some pending_parent_blocks =
    String_map.find_opt(
      block.Block.hash,
      state.Node.pending_blocks_by_previous,
    );
  pending_parent_blocks
  |> List.find_map(block_and_sigs => {
       let.some block = block_and_sigs.Node.block;
       check_block_and_signature_is_enough(state, block_and_sigs)
         ? Some(block) : None;
     });
};
// TODO: bad naming
let is_signable = (state, block) =>
  switch (state.Node.last_signed_block) {
  | Some(last_signed_block) =>
    last_signed_block.hash == block.Block.previous_hash
    && Int64.add(last_signed_block.block_height, 1L) == block.block_height
  | None => false
  };

// TODO: maybe should_be_last_signed_block
let is_last_signed_block = (state, block) =>
  switch (state.Node.last_signed_block) {
  | Some(last_signed_block) when last_signed_block.hash == block.Block.hash =>
    true
  | Some(last_signed_block) =>
    block.Block.block_height > last_signed_block.block_height
    && is_self_signed_block(state, ~hash=block.hash)
  | None => true
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
let append_signature = (state, update_state, ~hash, ~signature) => {
  let block_and_signature =
    switch (String_map.find_opt(hash, state.Node.pending_blocks)) {
    | Some(block_and_signatures) => block_and_signatures
    | None => {signatures: [], block: None, hash}
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
  let state = {...state, pending_blocks};
  let last_signed_block =
    switch (block_and_signature.block) {
    | Some(block) when is_last_signed_block(state, block) => Some(block)
    | _ => state.last_signed_block
    };
  update_state({...state, last_signed_block});
};

let add_block_to_pool = (state, update_state, block) => {
  let block_and_signatures =
    switch (String_map.find_opt(block.Block.hash, state.Node.pending_blocks)) {
    | Some(block_and_signatures) => block_and_signatures
    | None => {signatures: [], block: None, hash: block.hash}
    };
  if (Option.is_some(block_and_signatures.block)) {
    Error(`Block_already_in_the_pool);
  } else {
    let block_and_signatures = {...block_and_signatures, block: Some(block)};
    let pending_blocks_by_previous =
      switch (
        String_map.find_opt(
          block.previous_hash,
          state.pending_blocks_by_previous,
        )
      ) {
      | Some(pending_blocks_by_previous) => pending_blocks_by_previous
      | None => []
      };
    let state = {
      ...state,
      pending_blocks:
        String_map.add(
          block.hash,
          block_and_signatures,
          state.pending_blocks,
        ),
      pending_blocks_by_previous:
        String_map.add(
          block.previous_hash,
          pending_blocks_by_previous,
          state.pending_blocks_by_previous,
        ),
      last_signed_block:
        is_last_signed_block(state, block)
          ? Some(block) : state.last_signed_block,
    };
    Ok(update_state(state));
  };
};

let apply_block = (state, update_state, block) => {
  let.ok protocol = apply_block(state.Node.protocol, block);
  let state = update_state({...state, protocol});
  Ok(block |> Node.append_applied_block(state) |> update_state);
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
