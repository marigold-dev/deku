open Helpers;
open Core;
open Protocol;
open Node;

let get_initial_state = (~folder) => {
  let.await identity = Files.Identity.read(~file=folder ++ "/identity.json");

  let trusted_validator_membership_change_file =
    folder ++ "/trusted-validator-membership-change.json";

  let.await trusted_validator_membership_change_list =
    Files.Trusted_validators_membership_change.read(
      ~file=trusted_validator_membership_change_file,
    );
  let trusted_validator_membership_change =
    Trusted_validators_membership_change.Set.of_list(
      trusted_validator_membership_change_list,
    );
  let.await interop_context =
    Files.Interop_context.read(~file=folder ++ "/tezos.json");
  let.await validator_res =
    Tezos_interop.Consensus.fetch_validators(~context=interop_context);
  let validators =
    switch (validator_res) {
    | Ok(current_validators) =>
      current_validators
      |> List.mapi((i, validator) => {
           (
             Address.of_key_hash(validator),
             Printf.sprintf("http://localhost:444%d", i) |> Uri.of_string,
           )
         })
    | Error(err) => failwith(err)
    };

  let initial_validators_uri =
    List.fold_left(
      (validators_uri, (address, uri)) =>
        State.Address_map.add(address, uri, validators_uri),
      State.Address_map.empty,
      validators,
    );
  let persist_trusted_membership_change =
    Files.Trusted_validators_membership_change.write(
      ~file=trusted_validator_membership_change_file,
    );
  let node =
    State.make(
      ~identity,
      ~trusted_validator_membership_change,
      ~interop_context,
      ~data_folder=folder,
      ~initial_validators_uri,
      ~persist_trusted_membership_change,
    );
  let node = {
    ...node,
    protocol: {
      ...node.protocol,
      validators:
        List.fold_left(
          (validators, (address, _)) =>
            Validators.add({address: address}, validators),
          Validators.empty,
          validators,
        ),
    },
  };
  let state_bin = folder ++ "/state.bin";
  let.await state_bin_exists = Lwt_unix.file_exists(state_bin);

  let.await protocol =
    if (state_bin_exists) {
      Files.State_bin.read(~file=state_bin);
    } else {
      await(node.protocol);
    };

  let prev_epoch_state_bin = folder ++ "/prev_epoch_state.bin";
  let.await prev_epoch_state_bin_exists =
    Lwt_unix.file_exists(prev_epoch_state_bin);
  let.await snapshots =
    if (state_bin_exists && prev_epoch_state_bin_exists) {
      let.await prev_protocol =
        Files.State_bin.read(~file=prev_epoch_state_bin);
      let (hash, data) = Protocol.hash(prev_protocol);
      let snapshots =
        Snapshots.add_snapshot(
          ~new_snapshot=Snapshots.{hash, data},
          ~block_height=prev_protocol.block_height,
          node.snapshots,
        )
        |> Snapshots.start_new_epoch;
      await(snapshots);
    } else {
      await(node.snapshots);
    };

  let node = {...node, snapshots, protocol};

  await(node);
};
