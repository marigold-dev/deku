open Protocol;
open Helpers;
open Node_state;

[@deriving (ord, yojson)]
type side_effect =
  | Broadcast_side_chain_operation(Operation.Side_chain.Self_signed.t)
  | Broadcast_signed_block(Multisig.t(Block.Self_signed.t))
  | Reset_block_timeout;

type endpoint_response = {
  state: Node_state.t,
  side_effects: list(side_effect),
};

module type Endpoint = {
  [@deriving yojson]
  type request;

  let path: string;
  let handle_request: (Node_state.t, request) => endpoint_response;
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

    if (state.pending_side_ops |> List.mem(signed_operation)) {
      Ok({state, side_effects: []});
    } else {
      Ok({
        state: {
          ...state,
          pending_side_ops: [signed_operation, ...state.pending_side_ops],
        },
        side_effects: [Broadcast_side_chain_operation(signed_operation)],
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
      Ok({state, side_effects: []});
    } else {
      Ok({
        state: {
          ...state,
          pending_main_ops: [operation, ...state.pending_main_ops],
        },
        side_effects: [],
      });
    };
  };
};

module Signed_block = {
  [@deriving yojson]
  type request = {
    signatures: list(Multisig.signature),
    block: Block.t,
  };

  let path = "/signed-block";
  let handle_request = (state, request) => {};
};

[@deriving yojson]
type main_chain_operation_request = Signed.t(Operation.Main_chain.t);

let main_chain_operation_request = (state, json) => {
  // TODO: exceptions here
  let.ok main_operation = main_chain_operation_request_of_yojson(json);

  let.assert () = (
    "invalid signature for main_chain",
    main_operation.key == state.identity.t,
  );

  // TODO: check if it's in the list, to avoid infinite recursion network stuff
  if (state.pending_main_ops |> List.mem(main_operation.data)) {
    Ok({state, side_effects: []});
  } else {
    Ok({
      state: {
        ...state,
        pending_main_ops: [main_operation.data, ...state.pending_main_ops],
      },
      side_effects: [],
    });
  };
};

[@deriving yojson]
type signed_block_request = Multisig.t(Block.Self_signed.t);

let is_valid_block = (state, block: Block.t) => {
  let validators = Validators.validators(state.protocol.validators);
  let.assert () = (
    "not made by a validator",
    validators
    |> List.exists((Validators.{address, _}) => block.author == address),
  );
  // let.assert () = (

  // );
  Ok();
};

let exists = (v, f) => List.exists(f, v);
let signed_block_request = (state, json) => {
  // TODO: exceptions here
  let.ok signed_block = signed_block_request_of_yojson(json);
  let block = Multisig.data(signed_block).data;

  // TODO: fn concat signatures
  // TODO: filter non validator signatures
  let.ok signed_block =
    switch (state.pending_block_ops |> Address_map.find_opt(block.author)) {
    | Some(signed_block') when Multisig.data(signed_block') == block =>
      Multisig.signatures(signed_block')
      |> fold_left_ok(
           (signed_block, signature) =>
             Multisig.verify_signature(signature, signed_block),
           signed_block,
         )
    | None => Ok(signed_block)
    | _ => Error("duplicated block for this height")
    };

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

  let.ok () = is_valid_block(state, block);

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
        pending_block_ops: Address_map.empty,
        pending_side_ops,
        pending_main_ops,
        protocol,
      };
    } else {
      state;
    };
  let side_effects =
    Address_map.mem(block.author, state.pending_block_ops)
      ? []
      : [
        Broadcast_signed_block(Signed.sign(~key=state.identity.key, block)),
      ];
  Ok({state, side_effects});
};

let state = ref(Obj.magic());
let timeout = ref(Obj.magic());

// let broadcast = state => {
//   let send_message =
//   Validators.validators(state.validators)
//   |> List.map((Validators.{uri, _}) => uri)
//   |> filter_p_limited_concurrency(() => assert(false));
// };
let x = (r: response) => {
  state := r.state;
  Lwt.async(() => {
    r.side_effects
    |> List.sort_uniq(compare_side_effect)
    |> Lwt_list.iter_p(side_effect =>
         switch (side_effect) {
         | Broadcast_side_chain_operation(side_chain_op) => ()
         | Broadcast_signed_block(_) => ()
         | Reset_block_timeout => ()
         }
       )
  });
};
