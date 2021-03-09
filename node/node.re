open Helpers;
open Protocol;

module Address_map = Map_with_yojson_make(Address);

[@deriving yojson]
type identity = {
  key: Address.key,
  t: Address.t,
  uri: Uri.t,
};

[@deriving yojson]
type status =
  | Stopped
  | Syncing({pending_blocks: list(Multisig.t(Block.t))})
  | In_sync;

[@deriving yojson]
type t = {
  identity,
  pending_side_ops: list(Operation.Side_chain.Self_signed.t),
  pending_main_ops: list(Operation.Main_chain.t),
  pending_block_ops: Address_map.t(Multisig.t(Block.t)),
  status,
  protocol: Protocol.t,
};

let make = (~identity) => {
  identity,
  pending_side_ops: [],
  pending_main_ops: [],
  pending_block_ops: Address_map.empty,
  status: Stopped,
  protocol: Protocol.empty,
};

type side_effect =
  | Main_operation
  | Side_operation({
      signature: string,
      operation: Operation.Side_chain.t,
    })
  | Valid_block({
      signatures: list(Multisig.signature),
      block: Block.t,
    });

type endpoint_response = {
  state: t,
  side_effect: option(side_effect),
};

module type Side_effect_endpoint = {
  [@deriving yojson]
  type request;
  let path: string;
  let handle_request: (t, request) => result(endpoint_response, string);
};

module Side_chain_operation = {
  [@deriving yojson]
  type request = {
    operation: Operation.Side_chain.t,
    signature: string,
  };

  let path = "/side-chain-operation";
  let handle_request = (state, request) => {
    // validations
    let.ok signed_operation =
      Operation.Side_chain.Self_signed.verify(
        ~key=Wallet.get_address(request.operation.source),
        ~signature=request.signature,
        request.operation,
      );
    // TODO: check if already in block
    if (state.pending_side_ops |> List.mem(signed_operation)) {
      Ok({state, side_effect: None});
    } else {
      Ok({
        state: {
          ...state,
          pending_side_ops: [signed_operation, ...state.pending_side_ops],
        },
        side_effect:
          Some(
            Side_operation({
              operation: signed_operation.data,
              signature: signed_operation.signature,
            }),
          ),
      });
    };
  };
};

module Main_chain_operation = {
  [@deriving yojson]
  type request = {
    operation: Operation.Main_chain.t,
    signature: string,
  };

  let path = "/main-chain-request";
  let handle_request = (state, request) => {
    let {operation, _} = request;
    // validations
    let.ok _ =
      Signed.verify(
        ~key=state.identity.t,
        ~signature=request.signature,
        operation,
      );

    if (state.pending_main_ops |> List.mem(operation)) {
      Ok({state, side_effect: None});
    } else {
      Ok({
        state: {
          ...state,
          pending_main_ops: [operation, ...state.pending_main_ops],
        },
        side_effect: Some(Main_operation),
      });
    };
  };
};

module Signed_block = {
  [@deriving yojson]
  type request = {
    signatures: list(Multisig.signature),
    // TODO: Block.of_yojson is doing some validations
    block: Block.t,
  };

  let append_signatures = (signatures, signed_block) =>
    signatures
    |> fold_left_ok(
         (signed_block, signature) =>
           // TODO: is this okay if it fails? seems bad
           Multisig.verify_signature(signature, signed_block),
         signed_block,
       );

  let path = "/signed-block";

  let handle_when_stopped = (_state, _request) => assert(false);
  let handle_when_syncing = (_pending_blocks, _state, _request) =>
    assert(false);
  let handle_when_in_sync = (state, request) => {
    let block = request.block;

    let.assert () = (
      "not made by a validator",
      Validators.validators(state.protocol.validators)
      |> List.exists((Validators.{address, _}) => block.author == address),
    );

    let.ok (should_broadcast, signed_block) =
      switch (state.pending_block_ops |> Address_map.find_opt(block.author)) {
      | Some(signed_block) when block == Multisig.data(signed_block) =>
        let.ok signed_block =
          append_signatures(request.signatures, signed_block);
        Ok((false, signed_block));
      | Some(_signed_block) => Error("duplicated block for this height")
      | None =>
        switch (request.signatures) {
        | [] => Error("no signature provided")
        | [sig_, ...sigs] =>
          let.ok signed_block =
            Signed.verify(~key=sig_.key, ~signature=sig_.signature, block);
          let.ok signed_block =
            signed_block |> Multisig.of_signed |> append_signatures(sigs);
          Ok((true, signed_block));
        }
      };
    let signed_block =
      Multisig.append_sign(~key=state.identity.key, signed_block);

    // check if enough signatures
    // TODO: think on how could concurrency + remove validator be abused to hijack this
    let is_signed_enough = {
      // TODO: this seems like a weird abstraction
      let number_of_validators =
        state.protocol.validators |> Validators.validators |> List.length;

      let needed_validators =
        Float.(to_int(ceil(of_int(number_of_validators) *. (2.0 /. 3.0))));
      // TODO: filter signatures to only validators
      let how_many_signed = Multisig.signatures(signed_block) |> List.length;
      how_many_signed >= needed_validators;
    };

    // TODO: assert block is valid
    let state =
      if (is_signed_enough) {
        let pending_side_ops =
          state.pending_side_ops
          |> List.filter(op => !List.mem(op, block.side_chain_ops));
        let pending_main_ops =
          state.pending_main_ops
          |> List.filter(op => !List.mem(op, block.main_chain_ops));
        let protocol = Protocol.apply_block(state.protocol, block);
        {
          ...state,
          status: In_sync,
          pending_block_ops: Address_map.empty,
          pending_side_ops,
          pending_main_ops,
          protocol,
        };
      } else {
        state;
      };
    let side_effect =
      should_broadcast
        ? Some(
            Valid_block({
              block: Multisig.data(signed_block),
              signatures: Multisig.signatures(signed_block),
            }),
          )
        : None;
    Ok({state, side_effect});
  };
  let handle_request = (state, request) => {
    switch (state.protocol.block_height == 0L, state.status) {
    | (true, _)
    | (_, In_sync) => handle_when_in_sync(state, request)
    | (_, Stopped) => handle_when_stopped(state, request)
    | (_, Syncing({pending_blocks})) =>
      handle_when_syncing(pending_blocks, state, request)
    };
  };
};

module type Read_endpoint = {
  [@deriving yojson]
  type request;
  [@deriving yojson]
  type response;
  let path: string;
  let handle_request: (t, request) => result(response, string);
};

module Block_by_height = {
  [@deriving yojson]
  type request = {height: int64};
  [@deriving yojson]
  type response = {
    signatures: list(Multisig.signature),
    block: Block.t,
  };

  let path = "/block-by-height";
  let handle_request = (state, {height}) => {
    let.ok multisig_block =
      state.protocol.blocks
      |> List.find_opt(block =>
           Multisig.data(block).Block.block_height == height
         )
      |> Option.to_result(~none="block not found");
    let signatures = Multisig.signatures(multisig_block);
    let block = Multisig.data(multisig_block);
    Ok({signatures, block});
  };
};

module Last_block = {
  [@deriving yojson]
  type request = unit;
  [@deriving yojson]
  type response = {
    signatures: list(Multisig.signature),
    block: Block.t,
  };

  let path = "/last-block-data";
  let handle_request = (state, ()) => {
    let.ok (signatures, block) =
      switch (state.protocol.blocks) {
      | [] => Error("no block")
      | [block, ..._] =>
        Ok((Multisig.signatures(block), Multisig.data(block)))
      };
    Ok({signatures, block});
  };
};

exception Error_status;
let post =
    (
      type req,
      module E: Side_effect_endpoint with type request = req,
      data,
      uri,
    ) => {
  open Cohttp;
  open Cohttp_lwt_unix;
  let body = E.request_to_yojson(data) |> Yojson.Safe.to_string;
  let uri = Uri.with_path(uri, E.path);
  let.await (response, _) = Client.post(~body=`String(body), uri);
  Code.code_of_status(response.status) |> Code.is_success
    ? await() : Lwt.fail(Error_status);
};

let get =
    (
      type req,
      type res,
      module E: Read_endpoint with type request = req and type response = res,
      data,
      uri,
    ) => {
  open Cohttp_lwt_unix;
  let body = E.request_to_yojson(data) |> Yojson.Safe.to_string;
  let uri = Uri.with_path(uri, E.path);
  let.await (response, body) = Client.post(~body=`String(body), uri);
  let response = {
    let.ok () = response.status == `OK ? Ok() : Error(`Invalid_status);
    let.ok body =
      switch (body) {
      | `String(string) => Ok(string)
      // TODO: handle properly streams
      | _ => Error(`Invalid_body)
      };
    try({
      let json = Yojson.Safe.from_string(body);
      // TODO: carry error
      E.response_of_yojson(json)
      |> Result.map_error(_ => `Failed_to_parse_body);
    }) {
    // TODO: log exception
    | _ => Error(`Failed_to_parse_body)
    };
  };
  await(response);
};
let broadcast = (state, endpoint, data) =>
  Validators.validators(state.protocol.validators)
  |> List.map((Validators.{uri, _}) => uri)
  |> Lwt_list.iter_s(uri =>
       Lwt.catch(
         () => post(endpoint, data, uri),
         // TODO: log exception
         _exn => await(),
       )
     );

let format_address = (fmt, addr) => {
  addr
  |> Address.to_yojson
  |> Yojson.Safe.to_string
  |> Format.fprintf(fmt, "%s");
};
let is_time_to_make_block = state => {
  let current_block_producer = Validators.current(state.protocol.validators);
  switch (current_block_producer) {
  | Some(current_block_producer) =>
    current_block_producer.address == state.identity.t
  | None => false
  };
};

let make_block = state => {
  let block =
    Block.make(
      ~author=state.identity.t,
      ~block_height=Int64.add(state.protocol.block_height, 1L),
      ~main_chain_ops=[],
      ~side_chain_ops=[],
    );
  // TODO: this clearly smell
  let signatures =
    Signed.sign(~key=state.identity.key, block)
    |> Multisig.of_signed
    |> Multisig.signatures;
  (signatures, block);
};
