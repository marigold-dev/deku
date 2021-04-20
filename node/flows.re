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

let request_previous_blocks = () => assert(false);
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
    is_signed_enough(state, ~hash=block.Block.hash),
  );
  let.ok state = apply_block(state, update_state, block);
  reset_timeout^();
  let state = clean(state, update_state, block);
  let state = update_state({...state, last_applied_block: Some(block)});
  if (is_latest(state, block)) {
    try_to_produce_block(state, update_state);
  } else {
    switch (find_next_block_to_apply(state, block)) {
    | Some(block) => block_added_to_the_pool(state, update_state, block)
    | None => Error(`Pending_blocks)
    };
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
      is_signed_enough(state, ~hash=block.hash),
    );
    request_previous_blocks();
  };

let received_block = (state, update_state, block) => {
  let.ok () =
    is_valid_block(state, block)
    |> Result.map_error(msg => `Invalid_block(msg));

  let.assert () = (
    `Already_known_block,
    !is_known_block(state, ~hash=block.Block.hash),
  );

  let.ok state = add_block_to_pool(state, update_state, block);

  block_added_to_the_pool(state, update_state, block);
};

let received_signature = (state, update_state, ~hash, ~signature) => {
  let.ok signature = is_valid_signature(~hash, ~signature);
  let.assert () = (
    `Already_known_signature,
    !is_known_signature(state, ~hash, ~signature),
  );

  let state = append_signature(state, update_state, ~hash, ~signature);

  let.assert () = (
    `Added_signature_not_signed_enough_to_request,
    is_signed_enough(state, ~hash),
  );

  switch (find_block_in_pool(state, ~hash)) {
  | Some(block) => block_added_to_the_pool(state, update_state, block)
  | None => request_block(~hash)
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

let is_applied_hash = (state, hash) =>
  String_map.mem(hash, state.Node.applied_blocks);

let find_block_by_hash = (state, hash) => {
  let.none () = String_map.find_opt(hash, state.Node.applied_blocks);
  let.some {block, _} = String_map.find_opt(hash, state.Node.pending_blocks);
  block;
};
