// TODO: does computers have multiple RTC nowadays?
// Can a core send a message and the other receive it in the past?
// TODO: should start signing before being in sync?

Random.self_init();
Mirage_crypto_rng_unix.initialize();

open Opium;
open Helpers;
open Protocol;
open Node;

let ignore_some_errors =
  fun
  | Error(#Flows.ignore) => Ok()
  | v => v;
let print_error = err => {
  open Format;
  switch (err) {
  | `Not_a_json => eprintf("Invalid json")
  | `Not_a_valid_request(err) => eprintf("Invalid request: %s", err)
  | `Added_signature_not_signed_enough_to_request =>
    eprintf("Added_signature_not_signed_enough_to_request")
  | `Already_known_block => eprintf("Already_known_block")
  | `Already_known_signature => eprintf("Already_known_signature")
  | `Block_not_signed_enough_to_apply =>
    eprintf("Block_not_signed_enough_to_apply")
  | `Invalid_block(string) => eprintf("Invalid_block(%s)", string)
  | `Invalid_block_when_applying => eprintf("Invalid_block_when_applying")
  | `Invalid_signature_for_this_hash =>
    eprintf("Invalid_signature_for_this_hash")
  | `Invalid_state_root_hash => eprintf("Invalid_state_root_hash")
  | `Not_current_block_producer => eprintf("Not_current_block_producer")
  | `Pending_blocks => eprintf("Pending_blocks")
  | `Added_block_not_signed_enough_to_desync =>
    eprintf("Added_block_not_signed_enough_to_desync")
  | `Invalid_nonce_signature => eprintf("Invalid_nonce_signature")
  | `Unknown_uri => eprintf("Unknown_uri")
  | `Invalid_address_on_main_operation =>
    eprintf("Invalid_address_on_main_operation")
  | `Failed_to_verify_payload => eprintf("Failed to verify payload signature")
  };
  eprintf("\n%!");
};
let update_state = state => {
  Server.set_state(state);
  state;
};

let handle_request =
    (
      type req,
      type res,
      module E:
        Networking.Request_endpoint with
          type request = req and type response = res,
      f,
    ) =>
  App.post(
    E.path,
    request => {
      let.await json = Request.to_json(request);
      let response = {
        let.ok json = Option.to_result(~none=`Not_a_json, json);
        let.ok request =
          E.request_of_yojson(json)
          |> Result.map_error(err => `Not_a_valid_request(err));
        f(update_state, request);
      };
      switch (response) {
      | Ok(response) =>
        let response = E.response_to_yojson(response);
        await(Response.of_json(~status=`OK, response));
      | Error(err) =>
        print_error(err);
        await(Response.make(~status=`Internal_server_error, ()));
      };
    },
  );

let handle_received_block_and_signature =
  handle_request(
    (module Networking.Block_and_signature_spec),
    (update_state, request) => {
      open Flows;
      let.ok () =
        received_block(Server.get_state(), update_state, request.block)
        |> ignore_some_errors;

      let.ok () =
        received_signature(
          Server.get_state(),
          update_state,
          ~hash=request.block.hash,
          ~signature=request.signature,
        )
        |> ignore_some_errors;
      Ok();
    },
  );
let handle_received_signature =
  handle_request(
    (module Networking.Signature_spec),
    (update_state, request) => {
      open Flows;
      let.ok () =
        received_signature(
          Server.get_state(),
          update_state,
          ~hash=request.hash,
          ~signature=request.signature,
        )
        |> ignore_some_errors;
      Ok();
    },
  );
let handle_block_by_hash =
  handle_request(
    (module Networking.Block_by_hash_spec),
    (_update_state, request) => {
      let block = Flows.find_block_by_hash(Server.get_state(), request.hash);
      Ok(block);
    },
  );

let handle_block_level =
  handle_request((module Networking.Block_level), (_update_state, _request) => {
    Ok({level: Flows.find_block_level(Server.get_state())})
  });

let handle_protocol_snapshot =
  handle_request(
    (module Networking.Protocol_snapshot),
    (_update_state, ()) => {
      let Node_state.{snapshots, _} = Server.get_state();
      Ok({
        snapshot: snd(snapshots.Snapshots.last_snapshot),
        snapshot_hash: fst(snapshots.last_snapshot),
        additional_blocks: snapshots.additional_blocks,
        last_block: snapshots.last_block,
        last_block_signatures:
          Signatures.to_list(snapshots.last_block_signatures),
      });
    },
  );
let handle_request_nonce =
  handle_request(
    (module Networking.Request_nonce),
    (update_state, {uri}) => {
      let nonce = Flows.request_nonce(Server.get_state(), update_state, uri);
      Ok({nonce: nonce});
    },
  );
let handle_register_uri =
  handle_request(
    (module Networking.Register_uri), (update_state, {uri, signature}) =>
    Flows.register_uri(Server.get_state(), update_state, ~uri, ~signature)
  );
let handle_receive_operation_gossip =
  handle_request(
    (module Networking.Operation_gossip),
    (update_state, request) => {
      Flows.received_operation(Server.get_state(), update_state, request);
      Ok();
    },
  );

let handle_trusted_validators_membership =
    (
      ~file,
      ~persist:
         (list(Trusted_validators_membership_change.t), ~file: string) =>
         Lwt.t(unit),
    ) =>
  handle_request(
    (module Networking.Trusted_validators_membership_change),
    (update_state, request) => {
    Flows.trusted_validators_membership(
      ~file,
      ~persist,
      Server.get_state(),
      update_state,
      request,
    )
  });

let handle_withdraw_proof =
  handle_request((module Networking.Withdraw_proof), (_, {operation_hash}) =>
    Ok(
      Flows.request_withdraw_proof(Server.get_state(), ~hash=operation_hash),
    )
  );
let handle_ticket_balance =
  handle_request(
    (module Networking.Ticket_balance),
    (_update_state, {ticket, address}) => {
      let state = Server.get_state();
      let amount = Flows.request_ticket_balance(state, ~ticket, ~address);
      Ok({amount: amount});
    },
  );

let folder = Sys.argv[1];
let trusted_validator_membership_change =
  folder ++ "/trusted-validator-membership-change.json";

let node = {
  let.await identity = Files.Identity.read(~file=folder ++ "/identity.json");
  let.await trusted_validator_membership_change_list =
    Files.Trusted_validators_membership_change.read(
      ~file=folder ++ "/trusted-validator-membership-change.json",
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
             switch (validator) {
             | Tezos_interop.Key.Ed25519(k) => k
             },
             Printf.sprintf("https://localhost:444%d", i) |> Uri.of_string,
           )
         })
    | Error(err) => failwith(err)
    };
  let initial_validators_uri =
    List.fold_left(
      (validators_uri, (address, uri)) =>
        Node_state.Address_map.add(address, uri, validators_uri),
      Node_state.Address_map.empty,
      validators,
    );
  let node =
    Node_state.make(
      ~identity,
      ~trusted_validator_membership_change,
      ~interop_context,
      ~data_folder=folder,
      ~initial_validators_uri,
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
      let.await () = Files.State_bin.write(node.protocol, ~file=state_bin);
      await(node.protocol);
    };
  Tezos_interop.Consensus.listen_operations(
    ~context=interop_context, ~on_operation=operation =>
    switch (
      Flows.received_main_operation(
        Server.get_state(),
        update_state,
        operation,
      )
    ) {
    | Ok () => ()
    | Error(err) => print_error(err)
    }
  );
  await({...node, protocol});
};
let node = node |> Lwt_main.run;
let () = Node.Server.start(~initial=node);

let _server =
  App.empty
  |> App.port(Node.Server.get_port() |> Option.get)
  |> handle_block_level
  |> handle_received_block_and_signature
  |> handle_received_signature
  |> handle_block_by_hash
  |> handle_protocol_snapshot
  |> handle_request_nonce
  |> handle_register_uri
  |> handle_receive_operation_gossip
  |> handle_withdraw_proof
  |> handle_ticket_balance
  |> handle_trusted_validators_membership(
       ~persist=Files.Trusted_validators_membership_change.write,
       ~file=trusted_validator_membership_change,
     )
  |> App.start
  |> Lwt_main.run;

let (forever, _) = Lwt.wait();
let () = Lwt_main.run(forever);
