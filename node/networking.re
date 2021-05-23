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
  open Cohttp;
  open Cohttp_lwt_unix;
  let body = request_to_yojson(data) |> Yojson.Safe.to_string;
  let uri = Uri.with_path(uri, path);
  let.await (response, body) = Client.post(~body=`String(body), uri);
  let.await body = Cohttp_lwt.Body.to_string(body);
  Code.code_of_status(response.status) |> Code.is_success
    ? await(body) : Lwt.fail(Error_status);
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

let broadcast = (endpoint, state, data) =>
  Validators.validators(state.protocol.validators)
  |> List.map((Validators.{uri, _}) => uri)
  |> Lwt_list.iter_s(uri =>
       Lwt.catch(
         () => post(endpoint, data, uri),
         // TODO: log exception
         _exn => await(),
       )
     );

[@deriving yojson]
type signature = {
  key: Address.t,
  signature: string,
};

module Signature_spec = {
  [@deriving yojson]
  type request = {
    hash: SHA256.hash,
    signature: Signature.t,
  };
  [@deriving yojson]
  type response = unit;
  let path = "/append-signature";
};

// module Block_spec = {
//   [@deriving yojson]
//   type request = Block.t;
//   let path = "/append-block";
// };

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
  type request = {hash: SHA256.hash};
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
    snapshot_hash: SHA256.hash,
    additional_blocks: list(Block.t),
    last_block: Block.t,
    // TODO: this is bad, Signatures.t is a private type and not a network one
    last_block_signatures: Signatures.t,
  };
  let path = "/protocol-snapshot";
};

let request_block_by_hash = request((module Block_by_hash_spec));
let request_protocol_snapshot = request((module Protocol_snapshot));
let broadcast_signature = broadcast((module Signature_spec));
let broadcast_block_and_signature =
  broadcast((module Block_and_signature_spec));
