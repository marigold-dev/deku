open Helpers;
open Protocol;
open Building_blocks;

type flag_node = [ | `Invalid_block | `Invalid_signature];
type ignore = [
  | `Already_known_block
  | `Added_block_not_signed_enough_to_desync
  | `Block_not_signed_enough_to_apply
  | `Already_known_signature
  | `Added_signature_not_signed_enough_to_request
  | `Not_current_block_producer
  | `Pending_blocks
];

// TODO: set this by server
let reset_timeout: ref(unit => unit) = ref(() => assert(false));
let get_state: ref(unit => Node_state.t) = ref(() => assert(false));
let set_state: ref(Node_state.t => unit) = ref(_ => assert(false));

// TODO: poor man recursion
let received_block':
  ref(
    (Node_state.t, Node_state.t => Node_state.t, Block.t) =>
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
      ],
    ),
  ) =
  ref(_ => assert(false));
let block_added_to_the_pool':
  ref(
    (Node_state.t, Node_state.t => Node_state.t, Block.t) =>
    result(
      unit,
      [
        | `Added_block_not_signed_enough_to_desync
        | `Block_not_signed_enough_to_apply
        | `Invalid_block_when_applying
        | `Invalid_state_root_hash
        | `Not_current_block_producer
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
let load_snapshot = snapshot => {
  open Networking.Protocol_snapshot;

  let.ok state =
    Node_state.load_snapshot(
      ~state_root_hash=snapshot.snapshot_hash,
      ~state_root=snapshot.snapshot,
      ~additional_blocks=snapshot.additional_blocks,
      ~last_block=snapshot.last_block,
      ~last_block_signatures=snapshot.last_block_signatures,
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

let request_previous_blocks = (state: Node_state.t, block) =>
  if (block.Block.state_root_hash == state.protocol.state_root_hash) {
    request_block(~hash=block.Block.previous_hash);
  } else if (! pending^) {
    pending := true;
    request_protocol_snapshot();
  };

let try_to_produce_block = (state, update_state) => {
  let.assert () = (
    `Not_current_block_producer,
    is_current_producer(state, ~key=state.identity.t),
  );

  // TODO: avoid spam? how?
  let block = produce_block(state);
  let signature = sign(~key=state.identity.key, block);
  let state =
    append_signature(state, update_state, ~signature, ~hash=block.hash);
  broadcast_block_and_signature(state, ~block, ~signature);
  Ok();
};

let try_to_sign_block = (state, update_state, block) =>
  if (is_signable(state, block)) {
    let signature = sign(~key=state.identity.key, block);
    broadcast_signature(state, ~hash=block.hash, ~signature);
    append_signature(state, update_state, ~hash=block.hash, ~signature);
  } else {
    state;
  };

let rec try_to_apply_block = (state: Node_state.t, update_state, block) => {
  let.assert () = (
    `Block_not_signed_enough_to_apply,
    Block_pool.is_signed(~hash=block.Block.hash, state.block_pool),
  );
  let.ok state = apply_block(state, update_state, block);
  reset_timeout^();
  let state = clean(state, update_state, block);
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
and block_added_to_the_pool = (state: Node_state.t, update_state, block) => {
  let state =
    // TODO: we could receive signatures as parameters
    switch (
      Block_pool.find_signatures(~hash=block.Block.hash, state.block_pool)
    ) {
    | Some(signatures) when Signatures.is_signed(signatures) =>
      let snapshots =
        Snapshots.append_block(
          ~pool=state.block_pool,
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

let received_signature =
    (state: Node_state.t, update_state, ~hash, ~signature) => {
  let.assert () = (
    `Invalid_signature_for_this_hash,
    // TODO: check if it's made by a known validator, avoid spam
    Signature.verify(~signature, hash),
  );
  let.assert () = (
    `Already_known_signature,
    !is_known_signature(state, ~hash, ~signature),
  );

  let state: Node_state.t =
    append_signature(state, update_state, ~hash, ~signature);

  let.assert () = (
    `Added_signature_not_signed_enough_to_request,
    Block_pool.is_signed(~hash, state.block_pool),
  );

  switch (Block_pool.find_block(~hash, state.block_pool)) {
  | Some(block) => block_added_to_the_pool(state, update_state, block)
  | None =>
    request_block(~hash);
    Ok();
  };
};

let received_operation =
    (
      state: Node_state.t,
      update_state,
      request: Networking.Operation_gossip.request,
    ) =>
  if (!List.mem(request.operation, state.pending_side_ops)) {
    Lwt.async(() => {
      let _state =
        update_state(
          Node_state.{
            ...state,
            pending_side_ops: [request.operation, ...state.pending_side_ops],
          },
        );
      let.await () = Networking.broadcast_operation_gossip(state, request);
      Lwt.return();
    });
  };

let received_main_operation = (state: Node_state.t, update_state, operation) => {
  switch (operation.Tezos_interop.Consensus.parameters) {
  // TODO: handle this properly
  | Update_root_hash(_) => Ok()
  | Deposit({ticket, amount, destination}) =>
    let.ok destination =
      switch (destination) {
      | Tezos_interop.Address.Implicit(Ed25519(destination)) =>
        Ok(Wallet.address_of_blake(destination))
      | _ => Error(`Invalid_address_on_main_operation)
      };
    let amount = Amount.of_int(Z.to_int(amount));
    let kind = Operation.Main_chain.Deposit({ticket, amount, destination});
    let operation =
      Operation.Main_chain.make(
        ~tezos_hash=operation.Tezos_interop.Consensus.hash,
        ~tezos_index=operation.index,
        ~kind,
      );
    if (!List.mem(operation, state.pending_main_ops)) {
      let _ =
        update_state(
          Node_state.{
            ...state,
            pending_main_ops: [operation, ...state.pending_main_ops],
          },
        );
      ();
    };
    Ok();
  };
};

let find_block_by_hash = (state: Node_state.t, hash) =>
  Block_pool.find_block(~hash, state.block_pool);

let find_block_level = state => {
  state.Node_state.protocol.block_height;
};

let request_nonce = (state, update_state, uri) => {
  // TODO: nonce size, think about this, 32 is just magic because of SHA256
  let nonce = Mirage_crypto_rng.generate(32) |> Cstruct.to_string;
  let _state =
    update_state(
      Node_state.{
        ...state,
        uri_state: Node_state.Uri_map.add(uri, nonce, state.uri_state),
      },
    );
  BLAKE2B.hash(nonce);
};

let register_uri = (state: Node_state.t, update_state, ~uri, ~signature) => {
  // TODO: check if it's a validator
  let.ok nonce =
    Node_state.Uri_map.find_opt(uri, state.uri_state)
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
        Node_state.Address_map.add(
          Signature.public_key(signature),
          uri,
          state.validators_uri,
        ),
    });
  Ok();
};
let request_withdraw_proof = (state: Node_state.t, ~hash) =>
  switch (state.recent_operation_results |> BLAKE2B.Map.find_opt(hash)) {
  | None => Networking.Withdraw_proof.Unknown_operation
  | Some(`Transaction) => Operation_is_not_a_withdraw
  | Some(`Withdraw(handle)) =>
    let last_block_hash = state.protocol.last_block_hash;
    /* TODO: possible problem with this solution
       if this specific handles_hash was never commited to Tezos
       then the withdraw will fail at Tezos */
    let handles_hash =
      switch (Block_pool.find_block(~hash=last_block_hash, state.block_pool)) {
      // this branch is unreachable
      // TODO: make this unreachable through the typesystem
      | None => assert(false)
      | Some(block) => block.Block.handles_hash
      };
    let proof = state.protocol.ledger |> Ledger.handles_find_proof(handle);
    Ok({handles_hash, handle, proof});
  };
let request_ticket_balance = (state, ~ticket, ~address) =>
  state.Node_state.protocol.ledger |> Ledger.balance(address, ticket);

let trusted_validators_membership =
    (~file, ~persist, state: Node_state.t, update_state, request) => {
  open Networking.Trusted_validators_membership_change;
  let {signature, payload: {address, action} as payload} = request;
  let payload_hash =
    payload |> payload_to_yojson |> Yojson.Safe.to_string |> BLAKE2B.hash;
  let.assert () = (
    `Failed_to_verify_payload,
    payload_hash |> Signature.verify(~signature),
  );
  let new_validators =
    switch (action) {
    | Add =>
      Trusted_validators_membership_change.Set.add(
        {action: Add, address},
        state.trusted_validator_membership_change,
      )
    | Remove =>
      Trusted_validators_membership_change.Set.remove(
        {action: Remove, address},
        state.trusted_validator_membership_change,
      )
    };
  let _: Node_state.t =
    update_state({
      ...state,
      trusted_validator_membership_change: new_validators,
    });
  Lwt.async(() =>
    persist(
      new_validators |> Trusted_validators_membership_change.Set.elements,
      ~file,
    )
  );
  Ok();
};
