open Helpers;
open Protocol;
open State;

module type Side_effect_endpoint = {
  [@deriving yojson]
  type request;
  let path: string;
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
  let.await (response, body) = Client.post(~body=`String(body), uri);
  let.await _ = Cohttp_lwt.Body.to_string(body);
  Code.code_of_status(response.status) |> Code.is_success
    ? await() : Lwt.fail(Error_status);
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
    hash: string,
    signature,
  };
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
    signature,
  };
  let path = "/append-block-and-signature";
};

module Block_by_height_spec = {
  [@deriving yojson]
  type request = {block_height: int64};
  [@deriving yojson]
  type response = {block: option(Block.t)};

  let path = "/block-by-height";
};

module Is_applied_block_hash_spec = {
  [@deriving yojson]
  type request = {hash: string};
  [@deriving yojson]
  type response = {is_valid: bool};
  let path = "/is-applied-block-hash";
};

let broadcast_signature = broadcast((module Signature_spec));
let broadcast_block_and_signature =
  broadcast((module Block_and_signature_spec));

// let send_block = post((module Block_spec));
