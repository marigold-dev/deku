/*

 */

// TODO: does computers have multiple RTC nowadays?
// Can a core send a message and the other receive it in the past?
// TODO: should start signing before being in sync?

Random.self_init();
Mirage_crypto_rng_unix.initialize();

open Opium;
open Helpers;
open Protocol;
open Node;
open Networking;

let ignore_some_errors =
  fun
  | Error(#Flows.ignore) => Ok()
  | v => v;
let handle_request =
    (
      type req,
      type res,
      module E:
        Request_endpoint with type request = req and type response = res,
      f,
    ) =>
  App.post(
    E.path,
    request => {
      let update_state = state => {
        Server.set_state(state);
        state;
      };
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
        switch (err) {
        | `Not_a_json => print_endline("Invalid json")
        | `Not_a_valid_request(err) =>
          print_endline("Invalid request:" ++ err)
        | _ => print_endline("unhandled")
        };
        await(Response.make(~status=`Internal_server_error, ()));
      };
    },
  );
let handle_received_block_and_signature =
  handle_request(
    (module Block_and_signature_spec),
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
    (module Signature_spec),
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
    (module Block_by_hash_spec),
    (_update_state, request) => {
      let block = Flows.find_block_by_hash(Server.get_state(), request.hash);
      Ok(block);
    },
  );

let handle_block_level =
  handle_request((module Block_level), (_update_state, _request) => {
    Ok({level: Flows.find_block_level(Server.get_state())})
  });

let handle_protocol_snapshot =
  handle_request(
    (module Protocol_snapshot),
    (_update_state, ()) => {
      let State.{snapshots, _} = Server.get_state();
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
    (module Request_nonce),
    (update_state, {uri}) => {
      let nonce = Flows.request_nonce(Server.get_state(), update_state, uri);
      Ok({nonce: nonce});
    },
  );
let handle_register_uri =
  handle_request((module Register_uri), (update_state, {uri, signature}) =>
    Flows.register_uri(Server.get_state(), update_state, ~uri, ~signature)
  );
let handle_data_to_smart_contract =
  handle_request(
    (module Data_to_smart_contract),
    (_, ()) => {
      let state = Server.get_state();
      let block = state.snapshots.last_block;
      let signatures_map =
        state.snapshots.last_block_signatures
        |> Signatures.to_list
        |> List.map(Signature.signature_to_b58check_by_address)
        |> List.to_seq
        |> State.Address_map.of_seq;
      let (validators, signatures) =
        state.protocol.validators
        |> Validators.to_list
        |> List.map(validator => validator.Validators.address)
        |> List.map(address =>
             (
               Tezos_interop.Key.Ed25519(address)
               |> Tezos_interop.Key.to_string,
               State.Address_map.find_opt(address, signatures_map),
             )
           )
        |> List.split;

      Ok({
        block_hash: block.hash,
        block_height: block.block_height,
        block_payload_hash: block.payload_hash,
        state_hash: block.state_root_hash,
        handles_hash: block.handles_hash,
        validators,
        signatures,
      });
    },
  );
let handle_receive_operation_gossip =
  handle_request(
    (module Operation_gossip),
    (update_state, request) => {
      Flows.received_operation(Server.get_state(), update_state, request);
      Ok();
    },
  );

let node = {
  let folder = Sys.argv[1];
  let.await identity = Files.Identity.read(~file=folder ++ "/identity.json");
  let.await validators =
    Files.Validators.read(~file=folder ++ "/validators.json");
  let.await interop_context =
    Files.Interop_context.read(~file=folder ++ "/tezos.json");
  let initial_validators_uri =
    List.fold_left(
      (validators_uri, (address, uri)) =>
        State.Address_map.add(address, uri, validators_uri),
      State.Address_map.empty,
      validators,
    );
  let node =
    State.make(
      ~identity,
      ~interop_context,
      ~data_folder=folder,
      ~initial_validators_uri,
    );
  let node = {
    ...node,
    protocol: {
      ...node.protocol,
      validators:
        List.fold_right(
          ((address, _)) => Validators.add({address: address}),
          validators,
          Validators.empty,
        ),
    },
  };
  let state_bin = folder ++ "/state.bin";
  let.await state_bin_exists = Lwt_unix.file_exists(state_bin);
  let protocol =
    if (state_bin_exists) {
      let ic = open_in_bin(state_bin);
      let protocol = Marshal.from_channel(ic);
      close_in(ic);
      protocol;
    } else {
      node.protocol;
    };
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
  |> handle_data_to_smart_contract
  |> handle_receive_operation_gossip
  |> App.start
  |> Lwt_main.run;

let (forever, _) = Lwt.wait();
let () = Lwt_main.run(forever);
