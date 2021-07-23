open Helpers;
open Protocol;

[@deriving yojson]
type identity = {
  key: Address.key,
  t: Address.t,
  uri: Uri.t,
};

module Address_map = Map.Make(Address);
module Uri_map = Map.Make(Uri);

type t = {
  identity,
  data_folder: string,
  pending_side_ops: list(Operation.Side_chain.t),
  pending_main_ops: list(Operation.Main_chain.t),
  block_pool: Block_pool.t,
  protocol: Protocol.t,
  snapshots: Snapshots.t,
  // networking
  // TODO: move this to somewhere else but the string means the nonce needed
  // TODO: someone right now can spam the network to prevent uri changes
  // TODO: clean this once in a while
  // TODO: clean after nonce is used
  uri_state: Uri_map.t(string),
  validators_uri: Address_map.t(Uri.t),
};

let make = (~identity, ~data_folder, ~initial_validators_uri) => {
  let initial_block = Block.genesis;
  let initial_protocol = Protocol.make(~initial_block);
  let initial_signatures =
    Signatures.make(~self_key=identity.t) |> Signatures.set_signed;

  let initial_block_pool =
    Block_pool.make(~self_key=identity.t)
    |> Block_pool.append_block(initial_block);
  let initial_snapshots = {
    let initial_snapshot = Protocol.hash(initial_protocol);
    Snapshots.make(~initial_snapshot, ~initial_block, ~initial_signatures);
  };
  {
    identity,
    data_folder,
    pending_side_ops: [],
    pending_main_ops: [],
    block_pool: initial_block_pool,
    protocol: initial_protocol,
    snapshots: initial_snapshots,
    // networking
    uri_state: Uri_map.empty,
    validators_uri: initial_validators_uri,
  };
};

let apply_block = (state, block) => {
  let.ok (protocol, new_snapshot) = apply_block(state.protocol, block);
  let state = {...state, protocol};
  Lwt.async(() =>
    Lwt_io.with_file(
      ~mode=Output,
      state.data_folder ++ "/state.bin",
      oc => {
        let protocol_bin = Marshal.to_string(state.protocol, []);
        let.await () = Lwt_io.write(oc, protocol_bin);
        Lwt_io.flush(oc);
      },
    )
  );
  switch (new_snapshot) {
  | Some(new_snapshot) =>
    let snapshots =
      Snapshots.update(
        ~new_snapshot,
        ~applied_block_height=state.protocol.block_height,
        state.snapshots,
      );
    Ok({...state, snapshots});
  | None => Ok(state)
  };
};

// TODO: duplicated code
let signatures_required = state => {
  let number_of_validators = Validators.length(state.protocol.validators);
  // TODO: properly filter and check signatures
  Float.(to_int(ceil(of_int(number_of_validators) *. (2.0 /. 3.0))));
};
let load_snapshot =
    (
      ~state_root_hash,
      ~state_root,
      ~additional_blocks,
      ~last_block,
      // TODO: this is bad, Signatures.t is a private type and not a network one
      ~last_block_signatures,
      t,
    ) => {
  let all_blocks =
    [last_block, ...additional_blocks]
    |> List.sort((a, b) =>
         Int64.(to_int(sub(a.Block.block_height, b.Block.block_height)))
       );
  let block_pool = {
    let block_pool =
      List.fold_left(
        (block_pool, block) => Block_pool.append_block(block, block_pool),
        t.block_pool,
        all_blocks,
      );
    let signatures_required = signatures_required(t);
    List.fold_left(
      (block_pool, signature) =>
        Block_pool.append_signature(
          ~signatures_required,
          ~hash=last_block.Block.hash,
          signature,
          block_pool,
        ),
      block_pool,
      last_block_signatures,
    );
  };
  let.assert () = (
    `Not_all_blocks_are_signed,
    List.for_all(
      block => Block_pool.is_signed(~hash=block.Block.hash, block_pool),
      all_blocks,
    ),
  );
  let.assert () = (
    `State_root_not_the_expected,
    // TODO: this List.hd will not fail, but it makes me anxious
    state_root_hash == List.hd(all_blocks).state_root_hash,
  );
  let.assert () = (
    `Snapshots_with_invalid_hash,
    BLAKE2B.verify(~hash=state_root_hash, state_root),
  );

  let of_yojson = [%of_yojson:
    (
      Ledger.t,
      Operation_side_chain_set.t,
      Validators.t,
      BLAKE2B.t,
      int64,
      BLAKE2B.t,
      BLAKE2B.t,
    )
  ];
  let (
    ledger,
    included_operations,
    validators,
    validators_hash,
    block_height,
    last_block_hash,
    state_root_hash,
  ) =
    // TODO: verify the hash
    state_root |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok;

  // TODO: this is clearly an abstraction leak

  let protocol =
    Protocol.{
      ledger,
      included_operations,
      validators,
      validators_hash,
      block_height,
      last_block_hash,
      state_root_hash,
      last_state_root_update: 0.0,
      last_applied_block_timestamp: 0.0,
    };
  let.ok protocol =
    fold_left_ok(
      (protocol, block) => {
        // TODO: ignore this may be really bad for snapshots
        let.ok (protocol, _new_hash) = Protocol.apply_block(protocol, block);
        Ok(protocol);
      },
      protocol,
      all_blocks,
    );
  //TODO: snapshots?
  Ok({...t, block_pool, protocol});
};
