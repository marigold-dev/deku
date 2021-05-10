open Helpers;
open Protocol;
open Building_blocks;

module Node = State;

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
        | `Invalid_block_height_when_applying
        | `Not_current_block_producer
        | `Pending_blocks
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
      let random_int = v => v |> Int32.of_int |> Random.int32 |> Int32.to_int;
      let state = get_state^();
      let validators = Validators.validators(state.protocol.validators);
      let validator =
        List.nth(validators, random_int(List.length(validators)));

      let.await {block} =
        Networking.request_block_by_hash({hash: hash}, validator.uri);
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

let request_previous_blocks = block =>
  request_block(~hash=block.Block.previous_hash);

// TODO: implement

let try_to_produce_block = (state, update_state) => {
  let.assert () = (
    `Not_current_block_producer,
    is_current_producer(state, ~key=state.identity.t),
  );

  // TODO: avoid spam? how?
  let block = produce_block(state);
  let signature = sign(~key=state.identity.key, ~hash=block.hash);
  let state =
    append_signature(state, update_state, ~signature, ~hash=block.hash);
  broadcast_block_and_signature(state, ~block, ~signature);
  Ok();
};
let rec try_to_apply_block = (state, update_state, block) => {
  let.assert () = (
    `Block_not_signed_enough_to_apply,
    Block_pool.is_signed(~hash=block.Block.hash, state.Node.block_pool),
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
  | Some(block) => block_added_to_the_pool(state, update_state, block)
  | None =>
    // TODO: should I try even if not in sync?
    try_to_produce_block(state, update_state)
  };
}

// TODO: this function has a bad name
and block_added_to_the_pool = (state, update_state, block) =>
  if (is_next(state, block)) {
    let state =
      if (is_signable(state, block)
          && !is_signed_by_self(state, ~hash=block.hash)
          && is_current_producer(state, ~key=block.author)) {
        let signature = sign(~key=state.identity.key, ~hash=block.hash);
        broadcast_signature(state, ~hash=block.hash, ~signature);
        append_signature(state, update_state, ~hash=block.hash, ~signature);
      } else {
        state;
      };
    try_to_apply_block(state, update_state, block);
  } else {
    let.assert () = (
      `Added_block_not_signed_enough_to_desync,
      Block_pool.is_signed(~hash=block.hash, state.block_pool),
    );
    request_previous_blocks(block);
    Ok();
  };

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

let received_block = (state, update_state, block) =>
  received_block(state, update_state, block);

let received_signature = (state, update_state, ~hash, ~signature) => {
  let.ok signature = is_valid_signature(~hash, ~signature);
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

let requested_block_by_height = (state, block_height) => {
  let.assert () = (
    `Invalid_block_height,
    is_valid_block_height(state, block_height),
  );
  find_applied_block_by_height(state, block_height)
  |> Option.to_result(~none=`Unknown_block_height);
};

let find_block_by_hash = (state, hash) =>
  Block_pool.find_block(~hash, state.Node.block_pool);

let is_signed_block_hash = (state, hash) =>
  Block_pool.is_signed(~hash, state.Node.block_pool);
