open Helpers;
open Protocol;

let is_valid_block = (state: Node_state.t, block) => {
  // TODO: check if it's made by a known validator?
  let is_all_operations_properly_signed = _block =>
    // TODO: move this out from of_yojson
    true;
  let.assert () = (
    Printf.sprintf(
      "new block has a lower block height (%Ld) than the current state (%Ld)",
      block.Block.block_height,
      state.protocol.block_height,
    ),
    block.Block.block_height >= state.protocol.block_height,
  );

  // TODO: should we just ignore this operations?
  let.assert () = (
    "some operation in the block is not properly signed",
    is_all_operations_properly_signed(block),
  );

  Ok();
};

let is_next = (state: Node_state.t, block) =>
  Protocol.is_next(state.protocol, block);

let has_next_block_to_apply = (state: Node_state.t, ~hash) =>
  Block_pool.find_next_block_to_apply(~hash, state.block_pool)
  |> Option.is_some;

let is_known_block = (state: Node_state.t, ~hash) =>
  Option.is_some(Block_pool.find_block(state.block_pool, ~hash));
let is_known_signature = (state: Node_state.t, ~hash, ~signature) => {
  let.default () = false;
  let.some signatures = Block_pool.find_signatures(~hash, state.block_pool);
  Some(Signatures.mem(signature, signatures));
};

let is_signed_by_self = (state: Node_state.t, ~hash) => {
  // TODO: for the name of this function being correct we shuold check it recursively
  let.default () = false;
  let.some signatures = Block_pool.find_signatures(~hash, state.block_pool);
  Some(Signatures.is_self_signed(signatures));
};

let is_current_producer = (state: Node_state.t, ~key) => {
  let.default () = false;
  let.some current_producer = get_current_block_producer(state.protocol);
  Some(current_producer.address == key);
};

// TODO: bad naming
// TODO: check if block must have published a new snapshot
let is_signable = (state: Node_state.t, block) => {
  // TODO: this is O(n*m) which is bad
  let is_known_main = main_op =>
    state.pending_main_ops |> List.exists(op => op == main_op);
  let all_main_ops_are_known =
    List.for_all(is_known_main, block.Block.main_chain_ops);
  is_next(state, block)
  && !is_signed_by_self(state, ~hash=block.hash)
  && is_current_producer(state, ~key=block.author)
  && !has_next_block_to_apply(state, ~hash=block.hash)
  && all_main_ops_are_known;
};

let sign = (~key, block) => Block.sign(~key, block);

let produce_block = (state: Node_state.t) =>
  Block.produce(
    ~state=state.protocol,
    ~author=state.identity.t,
    ~main_chain_ops=state.pending_main_ops,
    ~side_chain_ops=state.pending_side_ops,
  );

let is_valid_block_height = (state: Node_state.t, block_height) =>
  block_height >= 1L && block_height <= state.protocol.block_height;

let signatures_required = (state: Node_state.t) => {
  let number_of_validators = Validators.length(state.protocol.validators);
  // TODO: properly filter and check signatures
  Float.(to_int(ceil(of_int(number_of_validators) *. (2.0 /. 3.0))));
};

// mutations
let append_signature = (state: Node_state.t, update_state, ~hash, ~signature) => {
  let block_pool =
    Block_pool.append_signature(
      ~signatures_required=signatures_required(state),
      ~hash,
      signature,
      state.block_pool,
    );
  update_state({...state, block_pool});
};

let add_block_to_pool = (state: Node_state.t, update_state, block) => {
  let block_pool = Block_pool.append_block(block, state.block_pool);
  update_state({...state, block_pool});
};

let apply_block = (state: Node_state.t, update_state, block) => {
  let.ok state = Node_state.apply_block(state, block);
  Ok(update_state(state));
};

let clean = (state: Node_state.t, update_state, block) => {
  // TODO: this is the dumbest piece of code that I could write
  //       but now it should work
  let main_op_not_in_block = main_op =>
    !List.mem(main_op, block.Block.main_chain_ops);
  let side_op_not_in_block = side_op =>
    !List.mem(side_op, block.side_chain_ops);
  let pending_main_ops =
    state.pending_main_ops |> List.find_all(main_op_not_in_block);
  let pending_side_ops =
    state.pending_side_ops |> List.find_all(side_op_not_in_block);
  // TODO: clean old blocks and old signatures
  update_state({...state, pending_main_ops, pending_side_ops});
};

// networking functions

// TODO: maybe send to some specific nodes so that they should broadcast it?
// maybe have a random chance of re-broadcasting it to the network
let broadcast_signature = (state: Node_state.t, ~hash, ~signature) =>
  Lwt.async(() => Networking.broadcast_signature(state, {hash, signature}));
let broadcast_block_and_signature = (state, ~block, ~signature) =>
  Lwt.async(() => {
    let.await () = Lwt_unix.sleep(1.0);
    Networking.broadcast_block_and_signature(state, {block, signature});
  });

let find_random_validator_uri = (state: Node_state.t) => {
  let random_int = v => v |> Int32.of_int |> Random.int32 |> Int32.to_int;
  let validators = Validators.to_list(state.protocol.validators);
  let rec safe_validator_uri = () => {
    let validator =
      List.nth(validators, random_int(List.length(validators)));
    if (state.identity.t == validator.address) {
      safe_validator_uri();
    } else {
      // TODO: this blown up if there is no validator uri registered
      switch (
        Node_state.Address_map.find_opt(
          validator.address,
          state.validators_uri,
        )
      ) {
      | Some(uri) => uri
      | None => safe_validator_uri()
      };
    };
  };
  safe_validator_uri();
};
