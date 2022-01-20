open Helpers;
open Crypto;
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

let is_current_producer = (state, ~key_hash) => {
  let.default () = false;
  let.some current_producer = get_current_block_producer(state.Node.protocol);
  Some(current_producer.address == key_hash);
};

// TODO: verify reasonable times for this with benchmarking.
// currently set to [state_root_min_timeout] as a reasonable
// default.
let minimum_signable_time_between_epochs = 10.0;

// TODO: verify reasonable times for this with benchmarking.
// Currently set to [minimum_signable_time_between_epochs] + 10
// as a reasonable default.
let maximum_signable_time_between_epochs = 20.0;

let block_has_signable_state_root_hash = (~current_time, state, block) => {
  let protocol = state.Node.protocol;
  let time_since_last_epoch = current_time -. protocol.last_state_root_update;
  Crypto.(
    if (BLAKE2B.equal(block.Block.state_root_hash, protocol.state_root_hash)) {
      time_since_last_epoch <= maximum_signable_time_between_epochs;
    } else {
      BLAKE2B.equal(block.state_root_hash, state.next_state_root |> fst)
      && time_since_last_epoch >= minimum_signable_time_between_epochs;
    }
  );
};

// TODO: bad naming
// TODO: check if block must have published a new snapshot
let is_signable = (state, block) => {
  let {
    Node.trusted_validator_membership_change,
    protocol: {last_seen_membership_change_timestamp, _},
    _,
  } = state;
  let current_time = Unix.time();
  // TODO: this should not be hard coded here
  let next_allowed_membership_change_timestamp =
    last_seen_membership_change_timestamp +. 24. *. 60. *. 60.;

  let is_trusted_operation = operation =>
    switch (operation) {
    | Protocol.Operation.Core_tezos(_) =>
      List.exists(
        op => Protocol.Operation.equal(op, operation),
        state.pending_operations,
      )
    | Core_user(_) => true
    | Consensus(consensus_operation) =>
      current_time > next_allowed_membership_change_timestamp
      && (
        switch (consensus_operation) {
        | Add_validator(validator) =>
          Trusted_validators_membership_change.Set.mem(
            {address: validator.address, action: Add},
            trusted_validator_membership_change,
          )
        | Remove_validator(validator) =>
          Trusted_validators_membership_change.Set.mem(
            {address: validator.address, action: Remove},
            trusted_validator_membership_change,
          )
        }
      )
    };

  // TODO: this is O(n*m) which is bad
  let all_operations_are_trusted =
    List.for_all(is_trusted_operation, block.Block.operations);

  is_next(state, block)
  && !is_signed_by_self(state, ~hash=block.hash)
  && is_current_producer(state, ~key_hash=block.author)
  && !has_next_block_to_apply(state, ~hash=block.hash)
  && all_operations_are_trusted
  && block_has_signable_state_root_hash(~current_time, state, block);
};

let sign = (~key, block) => Block.sign(~key, block);

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
  let avoid_jitter = 1.0;
  current_time
  -. last_state_root_update
  -. avoid_jitter >= minimum_signable_time_between_epochs;
};

let produce_block = state => {
  let start_new_epoch =
    should_start_new_epoch(
      state.Node.protocol.last_state_root_update,
      Unix.time(),
    );
  let next_state_root_hash =
    if (start_new_epoch) {
      Some(state.Node.next_state_root |> fst);
    } else {
      None;
    };
  Block.produce(
    ~state=state.Node.protocol,
    ~author=state.identity.t,
    ~next_state_root_hash,
    ~operations=state.pending_operations,
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
  let operation_is_in_block = operation =>
    List.exists(
      op => Operation.equal(op, operation),
      block.Block.operations,
    );
  let pending_operations =
    List.find_all(
      operation => !operation_is_in_block(operation),
      state.State.pending_operations,
    );
  let trusted_validator_membership_change =
    List.fold_left(
      (trusted_validator_membership_change, operation) =>
        switch (operation) {
        | Operation.Core_tezos(_)
        | Core_user(_) => trusted_validator_membership_change
        | Consensus(Add_validator(validator)) =>
          Trusted_validators_membership_change.Set.remove(
            {address: validator.address, action: Add},
            trusted_validator_membership_change,
          )
        | Consensus(Remove_validator(validator)) =>
          Trusted_validators_membership_change.Set.remove(
            {address: validator.address, action: Remove},
            state.Node.trusted_validator_membership_change,
          )
        },
      state.Node.trusted_validator_membership_change,
      block.operations,
    );

  Lwt.async(() =>
    trusted_validator_membership_change
    |> Trusted_validators_membership_change.Set.elements
    |> state.persist_trusted_membership_change
  );

  // TODO: clean old blocks and old signatures
  update_state({
    ...state,
    trusted_validator_membership_change,
    pending_operations,
  });
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
