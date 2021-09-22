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
  trusted_validator_membership_change: Trusted_validators_membership_change.Set.t,
  interop_context: Tezos_interop.Context.t,
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
  // TODO: use proper variants in the future
  // TODO: this also needs to be cleaned in the future
  recent_operation_results:
    BLAKE2B.Map.t([ | `Transaction | `Withdraw(Ledger.Handle.t)]),
};

let make =
    (
      ~identity,
      ~trusted_validator_membership_change,
      ~interop_context,
      ~data_folder,
      ~initial_validators_uri,
    ) => {
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
    trusted_validator_membership_change,
    interop_context,
    data_folder,
    pending_side_ops: [],
    pending_main_ops: [],
    block_pool: initial_block_pool,
    protocol: initial_protocol,
    snapshots: initial_snapshots,
    // networking
    uri_state: Uri_map.empty,
    validators_uri: initial_validators_uri,
    recent_operation_results: BLAKE2B.Map.empty,
  };
};

let commit_state_hash = state =>
  Tezos_interop.Consensus.commit_state_hash(~context=state.interop_context);
let try_to_commit_state_hash = (~old_state, state, block, signatures) => {
  let signatures_map =
    signatures
    |> Signatures.to_list
    |> List.map(Signature.signature_to_tezos_signature_by_address)
    |> List.to_seq
    |> Address_map.of_seq;

  let validators =
    state.protocol.validators
    |> Validators.to_list
    |> List.map(validator =>
         Tezos_interop.Key.Ed25519(validator.Validators.address)
       );
  let signatures =
    old_state.protocol.validators
    |> Validators.to_list
    |> List.map(validator => validator.Validators.address)
    |> List.map(address =>
         (
           Tezos_interop.Key.Ed25519(address),
           Address_map.find_opt(address, signatures_map),
         )
       );

  Lwt.async(() => {
    /* TODO: solve this magic number
       the goal here is to prevent a bunch of nodes concurrently trying
       to update the state root hash */
    let.await () =
      state.identity.t == block.Block.author
        ? Lwt.return_unit : Lwt_unix.sleep(120.0);
    commit_state_hash(
      state,
      ~block_hash=block.hash,
      ~block_height=block.block_height,
      ~block_payload_hash=block.payload_hash,
      ~handles_hash=block.handles_hash,
      ~state_hash=block.state_root_hash,
      ~validators,
      ~signatures,
    );
  });
};
let apply_block = (state, block) => {
  let old_state = state;
  let.ok (protocol, new_snapshot, results) =
    apply_block(state.protocol, block);
  let recent_operation_results =
    List.fold_left(
      (results, (op, result)) =>
        BLAKE2B.Map.add(op.Operation.Side_chain.hash, result, results),
      state.recent_operation_results,
      results,
    );
  let state = {...state, protocol, recent_operation_results};
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
    switch (Block_pool.find_signatures(~hash=block.hash, state.block_pool)) {
    | Some(signatures) when Signatures.is_self_signed(signatures) =>
      try_to_commit_state_hash(~old_state, state, block, signatures)
    | _ => ()
    };
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
      Operation_main_chain_set.t,
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
    included_main_operations,
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
      included_main_operations,
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
        // TODO: ignore the result is also really bad
        let.ok (protocol, _new_hash, _result) =
          Protocol.apply_block(protocol, block);
        Ok(protocol);
      },
      protocol,
      all_blocks,
    );
  //TODO: snapshots?
  Ok({...t, block_pool, protocol});
};
