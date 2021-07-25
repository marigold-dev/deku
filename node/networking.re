open Helpers;
open Protocol;
open State;

module type Request_endpoint = {
  [@deriving yojson]
  type request;
  [@deriving yojson]
  type response;
  let path: string;
};

exception Error_status;
let request = (request_to_yojson, path, data, uri) => {
  open Piaf;
  let (let.await_ok) = (promise, f) => {
    let.await value = promise;
    switch (value) {
    | Ok(value) => f(value)
    // TODO: handle this properly
    | Error(_err) => Lwt.fail(Error_status)
    };
  };
  let.await_ok response = {
    let uri = Uri.with_path(uri, path);
    let body =
      request_to_yojson(data) |> Yojson.Safe.to_string |> Body.of_string;
    Client.Oneshot.post(~body, uri);
  };

  if (Status.is_successful(response.status)) {
    let.await_ok body = Piaf.Body.to_string(response.body);
    await(body);
  } else {
    Lwt.fail(Error_status);
  };
};

let post =
    (type req, module E: Request_endpoint with type request = req, data, uri) => {
  let.await _body = request(E.request_to_yojson, E.path, data, uri);
  await();
};
let request =
    (
      type req,
      type res,
      module E:
        Request_endpoint with type request = req and type response = res,
      data,
      uri,
    ) => {
  let.await body = request(E.request_to_yojson, E.path, data, uri);
  let response =
    Yojson.Safe.from_string(body) |> E.response_of_yojson |> Result.get_ok;
  await(response);
};

let broadcast_to_list = (endpoint, uris, data) =>
  uris
  |> Lwt_list.iter_s(uri =>
       Lwt.catch(
         () => post(endpoint, data, uri),
         // TODO: log exception
         _exn => await(),
       )
     );

let broadcast_to_validators = (endpoint, state, data) =>
  Validators.to_list(state.protocol.validators)
  |> List.filter_map((Validators.{address, _}) =>
       State.Address_map.find_opt(address, state.validators_uri)
     )
  |> (uris => broadcast_to_list(endpoint, uris, data));

// protocol endpoints
module Signature_spec = {
  [@deriving yojson]
  type request = {
    hash: BLAKE2B.t,
    signature: Signature.t,
  };
  [@deriving yojson]
  type response = unit;
  let path = "/append-signature";
};

module Block_and_signature_spec = {
  [@deriving yojson]
  type request = {
    block: Block.t,
    signature: Signature.t,
  };
  [@deriving yojson]
  type response = unit;
  let path = "/append-block-and-signature";
};

module Block_by_hash_spec = {
  [@deriving yojson]
  type request = {hash: BLAKE2B.t};
  [@deriving yojson]
  type response = option(Block.t);

  let path = "/block-by-hash";
};

module Protocol_snapshot = {
  [@deriving yojson]
  type request = unit;
  [@deriving yojson]
  type response = {
    snapshot: string,
    snapshot_hash: BLAKE2B.t,
    additional_blocks: list(Block.t),
    last_block: Block.t,
    // TODO: this is bad, Signatures.t is a private type and not a network one
    last_block_signatures: list(Signature.t),
  };
  let path = "/protocol-snapshot";
};

// networking endpoints
module Request_nonce = {
  [@deriving yojson]
  type request = {uri: Uri.t};
  [@deriving yojson]
  type response = {nonce: BLAKE2B.t};
  let path = "/request-nonce";
};

module Register_uri = {
  [@deriving yojson]
  type request = {
    uri: Uri.t,
    signature: Signature.t,
  };
  [@deriving yojson]
  type response = unit;
  let path = "/register-uri";
};

module Operation_gossip = {
  [@deriving yojson]
  type request = {operation: Operation.Side_chain.t};
  [@deriving yojson]
  type response = unit;
  let path = "/operation-gossip";
};

module Data_to_smart_contract = {
  [@deriving yojson]
  type request = unit;
  [@deriving yojson]
  type response = {
    block_hash: BLAKE2B.t,
    block_height: int64,
    block_payload_hash: BLAKE2B.t,
    state_hash: BLAKE2B.t,
    validators: list(string),
    signatures: list(option(string)),
  };
  let path = "/data-to-smart-contract";
};

let request_block_by_hash = request((module Block_by_hash_spec));
let request_protocol_snapshot = request((module Protocol_snapshot));
let request_nonce = request((module Request_nonce));
let request_register_uri = request((module Register_uri));
let broadcast_signature = broadcast_to_validators((module Signature_spec));
let broadcast_block_and_signature =
  broadcast_to_validators((module Block_and_signature_spec));
let broadcast_operation_gossip =
  broadcast_to_validators((module Operation_gossip));
let broadcast_operation_gossip_to_list =
  broadcast_to_list((module Operation_gossip));
