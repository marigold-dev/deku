// TODO: does computers have multiple RTC nowadays?
// Can a core send a message and the other receive it in the past?
// TODO: should start signing before being in sync?

open Cmdliner;
open Opium;
open Helpers;
open Node;
open Bin_common;

let ignore_some_errors =
  fun
  | Error(#Flows.ignore) => Ok()
  | v => v;
let print_error = err => {
  open Format;
  switch (err) {
  | `Added_block_has_lower_block_height =>
    eprintf("Added block has lower block height")
  | `Added_block_not_signed_enough_to_desync =>
    eprintf("Added_block_not_signed_enough_to_desync")
  | `Added_signature_not_signed_enough_to_request =>
    eprintf("Added_signature_not_signed_enough_to_request")
  | `Already_known_block => eprintf("Already_known_block")
  | `Already_known_signature => eprintf("Already_known_signature")
  | `Block_not_signed_enough_to_apply =>
    eprintf("Block_not_signed_enough_to_apply")
  | `Failed_to_verify_payload => eprintf("Failed to verify payload signature")
  | `Invalid_address_on_main_operation =>
    eprintf("Invalid_address_on_main_operation")
  | `Invalid_block(string) => eprintf("Invalid_block(%s)", string)
  | `Invalid_block_when_applying => eprintf("Invalid_block_when_applying")
  | `Invalid_nonce_signature => eprintf("Invalid_nonce_signature")
  | `Invalid_signature_author => eprintf("Invalid_signature_author")
  | `Invalid_signature_for_this_hash =>
    eprintf("Invalid_signature_for_this_hash")
  | `Invalid_state_root_hash => eprintf("Invalid_state_root_hash")
  | `Not_current_block_producer => eprintf("Not_current_block_producer")
  | `Not_a_json => eprintf("Invalid json")
  | `Not_a_valid_request(err) => eprintf("Invalid request: %s", err)
  | `Pending_blocks => eprintf("Pending_blocks")
  | `Unknown_uri => eprintf("Unknown_uri")
  | `Not_a_user_opertaion => eprintf("Not_a_user_opertaion")
  | `Not_consensus_operation => eprintf("Not_consensus_operation")
  | `Invalid_signature => eprintf("Invalid_signature")
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
      let State.{snapshots, _} = Server.get_state();
      Ok({
        snapshot: snapshots.current_snapshot,
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
let handle_receive_user_operation_gossip =
  handle_request(
    (module Networking.User_operation_gossip), (update_state, request) => {
    Flows.received_user_operation(
      Server.get_state(),
      update_state,
      request.user_operation,
    )
  });
let handle_receive_consensus_operation =
  handle_request(
    (module Networking.Consensus_operation_gossip), (update_state, request) => {
    Flows.received_consensus_operation(
      Server.get_state(),
      update_state,
      request.consensus_operation,
      request.signature,
    )
  });

let handle_trusted_validators_membership =
  handle_request(
    (module Networking.Trusted_validators_membership_change),
    (update_state, request) => {
    Flows.trusted_validators_membership(
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

let node = folder => {
  let node = Node_state.get_initial_state(~folder) |> Lwt_main.run;
  Tezos_interop.Consensus.listen_operations(
    ~context=node.Node.State.interop_context, ~on_operation=operation =>
    Flows.received_tezos_operation(
      Server.get_state(),
      update_state,
      operation,
    )
  );
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
    |> handle_receive_user_operation_gossip
    |> handle_receive_consensus_operation
    |> handle_withdraw_proof
    |> handle_ticket_balance
    |> handle_trusted_validators_membership
    |> App.start
    |> Lwt_main.run;

  let (forever, _) = Lwt.wait();
  Lwt_main.run(forever);
};

let node = {
  let folder_node = {
    let docv = "folder_node";
    let doc = "Path to the folder containing the node configuration data.";
    Arg.(required & pos(0, some(string), None) & info([], ~doc, ~docv));
  };

  Term.(const(node) $ folder_node);
};

let () = {
  Term.exit @@ Term.eval((node, Term.info("deku-node")));
};
