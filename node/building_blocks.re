open Helpers;
open Protocol;

module Node = State;

let is_valid_block = (state, block) => {
  // TODO: check if it's made by a known validator?
  let is_all_operations_properly_signed = _block =>
    // TODO: move this out from of_yojson
    true;
  let.assert () = (
    "new block has a lower block height",
    block.Block.block_height >= state.Node.protocol.block_height,
  );

  // TODO: should we just ignore this operations?
  let.assert () = (
    "some operation in the block is not properly signed",
    is_all_operations_properly_signed(block),
  );

  Ok();
};

let is_next = (state, block) => Protocol.is_next(state.Node.protocol, block);

let has_next_block_to_apply = (state, ~hash) =>
  Block_pool.find_next_block_to_apply(~hash, state.Node.block_pool)
  |> Option.is_some;

let is_known_block = (state, ~hash) =>
  Option.is_some(Block_pool.find_block(state.Node.block_pool, ~hash));
let is_known_signature = (state, ~hash, ~signature) => {
  let.default () = false;
  let.some signatures =
    Block_pool.find_signatures(~hash, state.Node.block_pool);
  Some(Signatures.mem(signature, signatures));
};

let is_signed_by_self = (state, ~hash) => {
  // TODO: for the name of this function being correct we shuold check it recursively
  let.default () = false;
  let.some signatures =
    Block_pool.find_signatures(~hash, state.Node.block_pool);
  Some(Signatures.is_self_signed(signatures));
};

let is_current_producer = (state, ~key) => {
  let.default () = false;
  let.some current_producer = get_current_block_producer(state.Node.protocol);
  Some(current_producer.address == key);
};

// TODO: bad naming
// TODO: check if block must have published a new snapshot
let is_signable = (state, block) =>
  is_next(state, block)
  && !is_signed_by_self(state, ~hash=block.hash)
  && is_current_producer(state, ~key=block.author)
  && !has_next_block_to_apply(state, ~hash=block.hash);

let sign = (~key, block) => Block.sign(~key, block);

let produce_block = state =>
  Block.produce(
    ~state=state.Node.protocol,
    ~author=state.identity.t,
    ~main_chain_ops=state.pending_main_ops,
    ~side_chain_ops=state.pending_side_ops,
  );

let is_valid_block_height = (state, block_height) =>
  block_height >= 1L && block_height <= state.Node.protocol.block_height;

let signatures_required = state => {
  let number_of_validators =
    Validators.length(state.Node.protocol.validators);
  // TODO: properly filter and check signatures
  Float.(to_int(ceil(of_int(number_of_validators) *. (2.0 /. 3.0))));
};

// mutations
let append_signature = (state, update_state, ~hash, ~signature) => {
  let block_pool =
    Block_pool.append_signature(
      ~signatures_required=signatures_required(state),
      ~hash,
      signature,
      state.Node.block_pool,
    );
  update_state({...state, block_pool});
};

let add_block_to_pool = (state, update_state, block) => {
  let block_pool = Block_pool.append_block(block, state.Node.block_pool);
  update_state({...state, block_pool});
};

let apply_block = (state, update_state, block) => {
  let.ok state = Node.apply_block(state, block);
  Ok(update_state(state));
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
  update_state({...state, pending_main_ops, pending_side_ops});
};

// networking functions

// TODO: maybe send to some specific nodes so that they should broadcast it?
// maybe have a random chance of re-broadcasting it to the network
let broadcast_signature = (state, ~hash, ~signature) =>
  Lwt.async(() => Networking.broadcast_signature(state, {hash, signature}));
let broadcast_block_and_signature = (state, ~block, ~signature) =>
  Lwt.async(() => {
    let.await () = Lwt_unix.sleep(1.0);
    Networking.broadcast_block_and_signature(state, {block, signature});
  });
