open Deku_protocol
open Deku_concepts
open Deku_stdlib

let read_json of_yojson ~env ~file =
  let open Eio.Path in
  let path = Eio.Stdenv.cwd env / file in
  let string = load path in
  let json = Yojson.Safe.from_string string in
  of_yojson json

let write_json to_yojson data ~env ~file =
  let open Eio.Path in
  let path = Eio.Stdenv.cwd env / file in
  let json = to_yojson data in
  Eio.Path.save ~create:(`Exclusive 0o600) path
  @@ Yojson.Safe.pretty_to_string json

module Utils = struct
  let make_rnd_nonce () =
    Stdlib.Random.bits64 () |> Int64.abs |> Z.of_int64 |> N.of_z |> Option.get
    |> Nonce.of_n
end

module Api = struct
  (* response from the API *)
  type level_response = { level : Level.t } [@@deriving of_yojson]

  let current_level ~sw ~env ~api_uri =
    let level_uri = Uri.with_path api_uri "/api/v1/chain/level" in
    let response = Piaf.Client.Oneshot.get ~sw env level_uri in
    let body =
      match response with
      | Error _ -> failwith "cannot connect to the API"
      | Ok res -> res.body
    in
    let body =
      match Piaf.Body.to_string body with
      | Error _ -> failwith "cannot parse the level response from the API"
      | Ok body -> body |> Yojson.Safe.from_string
    in
    let { level } = level_response_of_yojson body in
    level

  let submit_operation ~sw ~env ~operation ~api_uri =
    let node = Uri.with_path api_uri "/api/v1/operations" in
    let json = Operation.yojson_of_t operation |> Yojson.Safe.to_string in
    let body = Piaf.Body.of_string json in
    let post_result = Piaf.Client.Oneshot.post ~body ~sw env node in
    match post_result with
    | Ok _ -> ()
    | Error err -> failwith (Piaf.Error.to_string err)
end

module Wallet = struct
  type t = { address : Deku_crypto.Key_hash.t; priv_key : Deku_crypto.Secret.t }
  [@@deriving yojson]

  let read ~env ~file = read_json t_of_yojson ~env ~file
  let make address priv_key = { address; priv_key }
  let address w = w.address
  let priv_key w = w.priv_key
  let write wallet ~env ~file = write_json yojson_of_t wallet ~env ~file
end
