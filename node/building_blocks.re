open Helpers;
open Protocol;

module Node = State;

let is_valid_block = (state, block) => {
  // TODO: check if it's made by a known validator?
  let is_all_operations_properly_signed = _block =>
    // TODO: move this out from of_yojson
    true;
  let.assert () = (
    Printf.sprintf(
      "new block has a lower block height (%Ld) than the current state (%Ld)",
      block.Block.block_height,
      state.Node.protocol.block_height,
    ),
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

/** Blocks that change the state root epoch before
    this interval in seconds has passed are not signable.

    TODO: verify reasonable times for this with benchmarking.
          currently set to [state_root_min_timeout] as a reasonable
          default. */
let minimum_signable_time_between_epochs = 60.0;
/** Blocks that do not change the state root epoch after
    this interval has passed are not signable.

    TODO: verify reasonable times for this with benchmarking.
    Currently set to [minimum_signable_time_between_epochs] + 10
    as a reasonable default. */
let maximum_signable_time_between_epochs = 70.0;

/* Returns true if the block's state root hash and has not
    changed too early or too late. Specifically, either:
   - The block has the same state root hash as the current
      state root hash and the maximum_signable_time_between_epochs
      has not elapsed.
   - Or the block has the same state root hash as [next_state_root_hash]
     and the minimum_signable_time_between_epochs as elapsed.
   */
let block_has_signable_state_root_hash = (~current_time, state, block) => {
  let protocol = state.Node.protocol;
  let time_since_last_epoch = protocol.last_state_root_update -. current_time;

  let same_epoch = protocol.state_root_hash == block.Block.state_root_hash;
  let next_epoch = state.next_state_root_hash == block.state_root_hash;
  let min_epoch_timeout =
    time_since_last_epoch >= minimum_signable_time_between_epochs;
  let max_epoch_timeout =
    time_since_last_epoch <= maximum_signable_time_between_epochs;
  switch (same_epoch, next_epoch, min_epoch_timeout, max_epoch_timeout) {
  | (true, false, _, false) => true
  | (false, true, true, _) => true
  | (true, true, _, _) => failwith("unrecheable")
  | _ => false
  };
};

// TODO: bad naming
// TODO: check if block must have published a new snapshot
let is_signable = (state, block) => {
  // TODO: this is O(n*m) which is bad
  let is_known_main = main_op =>
    state.State.pending_main_ops |> List.exists(op => op == main_op);
  let all_main_ops_are_known =
    List.for_all(is_known_main, block.Block.main_chain_ops);
  let {
    Node.trusted_validator_membership_change,
    protocol: {last_seen_membership_change_timestamp, _},
    _,
  } = state;
  let current_time = Unix.time();
  // TODO: this should not be hard coded here
  let next_allowed_membership_change_timestamp =
    last_seen_membership_change_timestamp +. 24. *. 60. *. 60.;

  let contains_only_trusted_add_validator_op =
    List.for_all(h => {
      switch (h.Operation.Side_chain.kind) {
      | Add_validator(validator) =>
        Trusted_validators_membership_change.Set.mem(
          {address: validator.address, action: Add},
          trusted_validator_membership_change,
        )
        && current_time > next_allowed_membership_change_timestamp
      | Remove_validator(validator) =>
        Trusted_validators_membership_change.Set.mem(
          {address: validator.address, action: Remove},
          trusted_validator_membership_change,
        )
        && current_time > next_allowed_membership_change_timestamp
      | _ => true
      }
    });
  is_next(state, block)
  && !is_signed_by_self(state, ~hash=block.hash)
  && is_current_producer(state, ~key=block.author)
  && !has_next_block_to_apply(state, ~hash=block.hash)
  && all_main_ops_are_known
  && contains_only_trusted_add_validator_op(block.side_chain_ops)
  && block_has_signable_state_root_hash(~current_time, state, block);
};

let sign = (~key, block) => Block.sign(~key, block);

/** The state_root_min_timeout determines how often the block producer
    starts hashing a new state root. It is closely related to the
    state root epoch (see notes in [should_start_new_epoch]]).

    The faster hashing is, the shorter this time should be. Faster updates
    to the state root hash means shorter times to join the network, since a
    node only needs to download and apply the blocks from the previous state root
    update to the current block.
*/
let state_root_min_timeout = 60.0;

/** Calculates whether to start sending a new state root hash.

    The state root epoch is the interval (in blocks) between state root
    hash updates. Thus, a new epoch is triggered by applying a block with
    a new state root hash. The block producer decides when to send
    blocks with new state root hashes. To enforce that he does so on time,
    validators reject blocks with updates that occur to soon or too late
    (see [Protocol.apply]).

    The block producer uses this function to determine when to send a
    block with an updated state root hash.
*/
let should_start_new_epoch = (last_state_root_update, current_time) => {
  /** To prevent changing the validator just because of network jittering,
      a short margin of error is introduced, allowing the block producer to start
      hashing slightly before the state_root_min_timeout. We arbitrarily chose 1s,
      but any reasonable time will do.*/
  let avoid_jitter = 1.0;
  current_time
  -. last_state_root_update
  -. avoid_jitter >= state_root_min_timeout;
};

let produce_block = state => {
  let start_new_epoch =
    should_start_new_epoch(
      state.Node.protocol.last_state_root_update,
      Unix.time(),
    );
  let next_hashes =
    if (start_new_epoch) {
      // We can only produce a block in the next epoch
      // if we've finished the state root hash for that 
      // epoch.
      let.some state_root = State.Int_map.find_opt(
          state.current_epoch + 1,
          state.finished_state_root_hashes,
        );
      Some(Block.{
            state_root,
            validators: Validators.hash(state.Node.protocol.validators),
          });
    } else {
      None;
    };
  Block.produce(
    ~state=state.Node.protocol,
    ~next_hashes,
    ~author=state.identity.t,
    ~main_chain_ops=state.pending_main_ops,
    ~side_chain_ops=state.pending_side_ops,
  );
};

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
  //       but now it should work
  let main_op_not_in_block = main_op =>
    !List.mem(main_op, block.Block.main_chain_ops);
  let side_op_not_in_block = side_op =>
    !List.mem(side_op, block.side_chain_ops);
  let pending_main_ops =
    state.Node.pending_main_ops |> List.find_all(main_op_not_in_block);
  let pending_side_ops =
    state.pending_side_ops |> List.find_all(side_op_not_in_block);
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

let find_random_validator_uri = state => {
  let random_int = v => v |> Int32.of_int |> Random.int32 |> Int32.to_int;
  let validators = Validators.to_list(state.Node.protocol.validators);
  let rec safe_validator_uri = () => {
    let validator =
      List.nth(validators, random_int(List.length(validators)));
    if (state.Node.identity.t == validator.address) {
      safe_validator_uri();
    } else {
      // TODO: this blown up if there is no validator uri registered
      switch (
        Node.Address_map.find_opt(validator.address, state.validators_uri)
      ) {
      | Some(uri) => uri
      | None => safe_validator_uri()
      };
    };
  };
  safe_validator_uri();
};
