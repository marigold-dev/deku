open Helpers;
open Crypto;
open Protocol;
open Building_blocks;

module Node = State;

let write_state_to_file = (path, protocol) => {
  let protocol_bin = Marshal.to_string(protocol, []);
  Lwt.async(() =>
    Lwt_io.with_file(
      ~mode=Output,
      path,
      oc => {
        let.await () = Lwt_io.write(oc, protocol_bin);
        Lwt_io.flush(oc);
      },
    )
  );
};

type flag_node = [ | `Invalid_block | `Invalid_signature];
type ignore = [
  | `Added_block_not_signed_enough_to_desync
  | `Added_signature_not_signed_enough_to_request
  | `Already_known_block
  | `Already_known_signature
  | `Block_not_signed_enough_to_apply
  | `Not_current_block_producer
  | `Pending_blocks
  | `Added_block_has_lower_block_height
];

// TODO: set this by server
let reset_timeout: ref(unit => unit) = ref(() => assert(false));
let get_state: ref(unit => State.t) = ref(() => assert(false));
let set_state: ref(State.t => unit) = ref(_ => assert(false));

// TODO: poor man recursion
let received_block':
  ref(
    (Node.t, Node.t => Node.t, Block.t) =>
    result(
      unit,
      [
        | `Added_block_not_signed_enough_to_desync
        | `Already_known_block
        | `Block_already_in_the_pool
        | `Block_not_signed_enough_to_apply
        | `Invalid_block(string)
        | `Invalid_block_when_applying
        | `Invalid_state_root_hash
        | `Not_current_block_producer
        | `Pending_blocks
        | `Added_block_has_lower_block_height
      ],
    ),
  ) =
  ref(_ => assert(false));
let block_added_to_the_pool':
  ref(
    (Node.t, Node.t => Node.t, Block.t) =>
    result(
      unit,
      [
        | `Added_block_not_signed_enough_to_desync
        | `Block_not_signed_enough_to_apply
        | `Invalid_block_when_applying
        | `Invalid_state_root_hash
        | `Not_current_block_producer
        | `Added_block_has_lower_block_height
      ],
    ),
  ) =
  ref(_ => assert(false));

let rec request_block_by_hash = (tries, ~hash) => {
  // TODO: magic number
  if (tries > 20) {
    raise(Not_found);
  };
  Lwt.catch(
    () => {
      let state = get_state^();
      let validator_uri = find_random_validator_uri(state);

      let.await block =
        Networking.request_block_by_hash({hash: hash}, validator_uri);
      // TODO: validate hash
      await(Option.get(block));
    },
    _exn => {
      Printexc.print_backtrace(stdout);
      request_block_by_hash(tries + 1, ~hash);
    },
  );
};

let request_block = (~hash) =>
  Lwt.async(() => {
    // TODO: check if block was already requested
    let.await block = request_block_by_hash(0, ~hash);
    let state = get_state^();
    switch (
      received_block'^(
        state,
        state => {
          set_state^(state);
          state;
        },
        block,
      )
    ) {
    | Ok () => await()
    // TODO: error?
    | Error(_err) => await()
    };
  });
let rec request_protocol_snapshot = tries => {
  // TODO: magic number
  if (tries > 20) {
    raise(Not_found);
  };
  Lwt.catch(
    () => {
      let state = get_state^();
      let validator_uri = find_random_validator_uri(state);
      // TODO: validate hash and signatures
      Networking.request_protocol_snapshot((), validator_uri);
    },
    _exn => {
      Printexc.print_backtrace(stdout);
      request_protocol_snapshot(tries + 1);
    },
  );
};
Lwt.async_exception_hook :=
  (
    exn => {
      Printexc.to_string(exn) |> Format.eprintf("global_exception: %s\n%!");
      Printexc.print_backtrace(stderr);
    }
  );
let pending = ref(false);
let load_snapshot = snapshot_data => {
  open Networking.Protocol_snapshot;
  let.ok state =
    Node.load_snapshot(
      ~snapshot=snapshot_data.snapshot,
      ~additional_blocks=snapshot_data.additional_blocks,
      ~last_block=snapshot_data.last_block,
      ~last_block_signatures=snapshot_data.last_block_signatures,
      get_state^(),
    );
  Ok(set_state^(state));
};
let request_protocol_snapshot = () =>
  Lwt.async(() => {
    let.await snapshot = request_protocol_snapshot(0);
    let _result = load_snapshot(snapshot);
    // TODO: log this
    await();
  });

let request_previous_blocks = (state, block) =>
  if (block_matches_current_state_root_hash(state, block)
      || block_matches_next_state_root_hash(state, block)) {
    request_block(~hash=block.Block.previous_hash);
  } else if (! pending^) {
    pending := true;
    request_protocol_snapshot();
  };

let try_to_produce_block = (state, update_state) => {
  let.assert () = (
    `Not_current_block_producer,
    is_current_producer(state, ~key_hash=state.identity.t),
  );

  // TODO: avoid spam? how?
  let block = produce_block(state);
  let signature = sign(~key=state.identity.secret, block);
  let state =
    append_signature(state, update_state, ~signature, ~hash=block.hash);
  broadcast_block_and_signature(state, ~block, ~signature);
  Ok();
};

let try_to_sign_block = (state, update_state, block) =>
  if (is_signable(state, block)) {
    let signature = sign(~key=state.identity.secret, block);
    broadcast_signature(state, ~hash=block.hash, ~signature);
    append_signature(state, update_state, ~hash=block.hash, ~signature);
  } else {
    state;
  };

let commit_state_hash = state =>
  Tezos_interop.Consensus.commit_state_hash(
    ~context=state.Node.interop_context,
  );
let try_to_commit_state_hash = (~prev_validators, state, block, signatures) => {
  open Node;
  let signatures_map =
    signatures
    |> Signatures.to_list
    |> List.map(signature => {
         let address = Signature.address(signature);
         let key = Signature.public_key(signature);
         let signature = Signature.signature(signature);
         (address, (key, signature));
       })
    |> List.to_seq
    |> Address_map.of_seq;

  let validators =
    state.protocol.validators
    |> Validators.to_list
    |> List.map(validator =>
         Core.Address.to_key_hash(validator.Validators.address)
       );
  let signatures =
    prev_validators
    |> Validators.to_list
    |> List.map(validator => validator.Validators.address)
    |> List.map(address => Address_map.find_opt(address, signatures_map));

  Lwt.async(() => {
    /* TODO: solve this magic number
       the goal here is to prevent a bunch of nodes concurrently trying
       to update the state root hash */
    let.await () =
      state.identity.t == block.Block.author
        ? Lwt.return_unit : Lwt_unix.sleep(120.0);
    commit_state_hash(
      state,
      ~block_height=block.block_height,
      ~block_payload_hash=block.payload_hash,
      ~handles_hash=block.handles_hash,
      ~state_hash=block.state_root_hash,
      ~validators,
      ~signatures,
    );
  });
};

let rec try_to_apply_block = (state, update_state, block) => {
  let.assert () = (
    `Block_not_signed_enough_to_apply,
    Block_pool.is_signed(~hash=block.Block.hash, state.Node.block_pool),
  );

  // TODO: in the future, we should stop the chain if this assert fails
  let.assert () = (
    `Invalid_state_root_hash,
    block_matches_current_state_root_hash(state, block)
    || block_matches_next_state_root_hash(state, block),
  );

  let prev_protocol = state.protocol;
  let is_new_state_root_hash =
    !BLAKE2B.equal(state.protocol.state_root_hash, block.state_root_hash);

  let.ok state = apply_block(state, update_state, block);
  write_state_to_file(state.Node.data_folder ++ "/state.bin", state.protocol);

  reset_timeout^();
  let state = clean(state, update_state, block);

  if (is_new_state_root_hash) {
    // Save the hash that will become the next state root
    // to disk so if the node goes offline before finishing
    // hashing it, it can pick up where it left off.
    write_state_to_file(
      state.data_folder ++ "/prev_epoch_state.bin",
      prev_protocol,
    );
    switch (Block_pool.find_signatures(~hash=block.hash, state.block_pool)) {
    | Some(signatures) when Signatures.is_self_signed(signatures) =>
      try_to_commit_state_hash(
        ~prev_validators=prev_protocol.validators,
        state,
        block,
        signatures,
      )
    | _ => ()
    };
  };

  switch (
    Block_pool.find_next_block_to_apply(
      ~hash=block.Block.hash,
      state.block_pool,
    )
  ) {
  | Some(block) =>
    let state = try_to_sign_block(state, update_state, block);
    try_to_apply_block(state, update_state, block);
  | None =>
    // TODO: should I try even if not in sync?
    try_to_produce_block(state, update_state)
  };
}

// TODO: this function has a bad name
and block_added_to_the_pool = (state, update_state, block) => {
  let state =
    // TODO: we could receive signatures as parameters
    switch (
      Block_pool.find_signatures(
        ~hash=block.Block.hash,
        state.Node.block_pool,
      )
    ) {
    | Some(signatures) when Signatures.is_signed(signatures) =>
      let snapshots =
        Snapshots.append_block(
          ~pool=state.Node.block_pool,
          (block, signatures),
          state.snapshots,
        );
      {...state, snapshots};
    | Some(_signatures) => state
    | None =>
      // TODO: this will never happen but meeh
      state
    };
  if (is_next(state, block)) {
    let state = try_to_sign_block(state, update_state, block);
    try_to_apply_block(state, update_state, block);
  } else {
    let.assert () = (
      `Added_block_not_signed_enough_to_desync,
      Block_pool.is_signed(~hash=block.hash, state.block_pool),
    );
    // TODO: block.hash == state.protocol.last_block_hash should be Ok()
    let.assert () = (
      `Added_block_has_lower_block_height,
      block.block_height > state.protocol.block_height,
    );
    switch (
      // TODO: this breaks request recursion
      Block_pool.find_block(
        ~hash=block.previous_hash,
        state.block_pool,
      )
    ) {
    | Some(block) => block_added_to_the_pool(state, update_state, block)
    | None =>
      request_previous_blocks(state, block);
      Ok();
    };
  };
};

block_added_to_the_pool' := block_added_to_the_pool;

let received_block = (state, update_state, block) => {
  let.ok () =
    is_valid_block(state, block)
    |> Result.map_error(msg => `Invalid_block(msg));

  let.assert () = (
    `Already_known_block,
    !is_known_block(state, ~hash=block.Block.hash),
  );

  let state = add_block_to_pool(state, update_state, block);
  block_added_to_the_pool(state, update_state, block);
};

let () = received_block' := received_block;

let received_signature = (state, update_state, ~hash, ~signature) => {
  let.assert () = (
    `Invalid_signature_for_this_hash,
    // TODO: check if it's made by a known validator, avoid spam
    Signature.verify(~signature, hash),
  );
  let.assert () = (
    `Already_known_signature,
    !is_known_signature(state, ~hash, ~signature),
  );

  let state = append_signature(state, update_state, ~hash, ~signature);

  let.assert () = (
    `Added_signature_not_signed_enough_to_request,
    Block_pool.is_signed(~hash, state.Node.block_pool),
  );

  switch (Block_pool.find_block(~hash, state.Node.block_pool)) {
  | Some(block) => block_added_to_the_pool(state, update_state, block)
  | None =>
    request_block(~hash);
    Ok();
  };
};

let parse_internal_tezos_transaction = transaction =>
  switch (transaction) {
  | Tezos_interop.Consensus.Update_root_hash(_) => Error(`Update_root_hash)
  | Tezos_interop.Consensus.Deposit({ticket, amount, destination}) =>
    let amount = Core.Amount.of_int(Z.to_int(amount));
    Ok(Core.Tezos_operation.Tezos_deposit({destination, amount, ticket}));
  };
let parse_internal_tezos_transactions = tezos_internal_transactions =>
  List.filter_map(
    transaction =>
      switch (parse_internal_tezos_transaction(transaction)) {
      | Ok(core_tezos_internal_transactions) =>
        Some(core_tezos_internal_transactions)
      | Error(`Update_root_hash) => None
      },
    tezos_internal_transactions,
  );

let received_tezos_operation = (state, update_state, tezos_interop_operation) => {
  open Protocol.Operation;
  let Tezos_interop.Consensus.{hash, transactions} = tezos_interop_operation;
  let tezos_operation =
    Core.Tezos_operation.make({
      tezos_operation_hash: hash,
      internal_operations: parse_internal_tezos_transactions(transactions),
    });

  let operation = Core_tezos(tezos_operation);
  let _: State.t =
    update_state(
      Node.{
        ...state,
        pending_operations: [operation, ...state.pending_operations],
      },
    );
  ();
};
let received_user_operation = (state, update_state, user_operation) => {
  open Protocol.Operation;
  let operation = Core_user(user_operation);
  let operation_exists =
    List.exists(op => equal(op, operation), state.Node.pending_operations);
  if (!operation_exists) {
    Lwt.async(() =>
      Networking.broadcast_user_operation_gossip(
        state,
        {user_operation: user_operation},
      )
    );
    let _: State.t =
      update_state(
        Node.{
          ...state,
          pending_operations: [operation, ...state.pending_operations],
        },
      );
    ();
  };
  Ok();
};
let received_consensus_operation =
    (state, update_state, consensus_operation, signature) => {
  open Protocol.Operation;
  let.assert () = (
    `Invalid_signature,
    Consensus.verify(state.Node.identity.key, signature, consensus_operation),
  );

  let operation = Consensus(consensus_operation);
  let _: State.t =
    update_state(
      Node.{
        ...state,
        pending_operations: [operation, ...state.pending_operations],
      },
    );
  Ok();
};

let find_block_by_hash = (state, hash) =>
  Block_pool.find_block(~hash, state.Node.block_pool);

let find_block_level = state => {
  state.State.protocol.block_height;
};

let request_nonce = (state, update_state, uri) => {
  // TODO: nonce size, think about this, 32 is just magic because of SHA256
  let nonce = Random.generate(32) |> Cstruct.to_string;
  let _state =
    update_state(
      Node.{
        ...state,
        uri_state: Node.Uri_map.add(uri, nonce, state.uri_state),
      },
    );
  BLAKE2B.hash(nonce);
};

let register_uri = (state, update_state, ~uri, ~signature) => {
  // TODO: check if it's a validator
  let.ok nonce =
    Node.Uri_map.find_opt(uri, state.Node.uri_state)
    |> Option.to_result(~none=`Unknown_uri);
  let.assert () = (
    `Invalid_nonce_signature,
    Signature.verify(~signature, BLAKE2B.hash(nonce)),
  );

  // TODO: broadcast this to other nodes? Maybe even to main chain
  let _state =
    update_state({
      ...state,
      validators_uri:
        Node.Address_map.add(
          Signature.address(signature),
          uri,
          state.validators_uri,
        ),
    });
  Ok();
};
let request_withdraw_proof = (state, ~hash) =>
  switch (state.Node.recent_operation_receipts |> BLAKE2B.Map.find_opt(hash)) {
  | None => Networking.Withdraw_proof.Unknown_operation
  | Some(Receipt_tezos_withdraw(handle)) =>
    let last_block_hash = state.Node.protocol.last_block_hash;
    /* TODO: possible problem with this solution
       if this specific handles_hash was never commited to Tezos
       then the withdraw will fail at Tezos */
    let handles_hash =
      switch (
        Block_pool.find_block(~hash=last_block_hash, state.Node.block_pool)
      ) {
      // this branch is unreachable
      // TODO: make this unreachable through the typesystem
      | None => assert(false)
      | Some(block) => block.Block.handles_hash
      };
    let proof =
      state.Node.protocol.core_state
      |> Core.State.ledger
      |> Ledger.handles_find_proof(handle);
    Ok({handles_hash, handle, proof});
  };
let request_ticket_balance = (state, ~ticket, ~address) =>
  state.Node.protocol.core_state
  |> Core.State.ledger
  |> Ledger.balance(address, ticket);

let trusted_validators_membership = (state, update_state, request) => {
  open Networking.Trusted_validators_membership_change;
  let {signature, payload: {address, action} as payload} = request;
  let payload_hash =
    payload |> payload_to_yojson |> Yojson.Safe.to_string |> BLAKE2B.hash;
  let.assert () = (
    `Invalid_signature_author,
    Core.Address.compare(state.Node.identity.t, Signature.address(signature))
    == 0,
  );
  let.assert () = (
    `Failed_to_verify_payload,
    payload_hash |> Signature.verify(~signature),
  );
  let new_validators =
    switch (action) {
    | Add =>
      Trusted_validators_membership_change.Set.add(
        {action: Add, address},
        state.Node.trusted_validator_membership_change,
      )
    | Remove =>
      Trusted_validators_membership_change.Set.add(
        {action: Remove, address},
        state.Node.trusted_validator_membership_change,
      )
    };
  let _: State.t =
    update_state({
      ...state,
      trusted_validator_membership_change: new_validators,
    });
  Lwt.async(() =>
    state.persist_trusted_membership_change(
      new_validators |> Trusted_validators_membership_change.Set.elements,
    )
  );
  Ok();
};
