open Helpers
open Node
open State
open Consensus

exception Invalid_json of string

let read_json of_yojson ~file =
  let%await string = Lwt_io.with_file ~mode:Input file Lwt_io.read in
  let json = Yojson.Safe.from_string string in
  match of_yojson json with
  | Ok data -> await data
  | Error error -> raise (Invalid_json error)

let write_json to_yojson data ~file =
  Lwt_io.with_file ~mode:Output file (fun oc ->
      Lwt_io.write oc (Yojson.Safe.pretty_to_string (to_yojson data)))

module Identity = struct
  let read = read_json identity_of_yojson

  let write = write_json identity_to_yojson
end

module Wallet = struct
  type t = {
    address : Crypto.Key_hash.t;
    priv_key : Crypto.Secret.t;
  }
  [@@deriving yojson]

  let read = read_json of_yojson

  let write = write_json to_yojson
end

module Interop_context = struct
  module Secret = struct
    include Crypto.Secret

    let of_yojson = function
      | `String string ->
        of_string string |> Option.to_result ~none:"invalid secret"
      | _ -> Error "expected a string"

    let to_yojson t = `String (to_string t)
  end

  type t = {
    rpc_node : Uri.t;
    secret : Secret.t;
    consensus_contract : Tezos.Address.t;
    discovery_contract : Tezos.Address.t;
    required_confirmations : int;
  }
  [@@deriving yojson]

  let read = read_json of_yojson

  let write = write_json to_yojson
end

module State_bin = struct
  let read ~file = Lwt_io.with_file ~mode:Input file Lwt_io.read_value

  let write protocol ~file =
    Lwt_io.with_file ~mode:Output file (fun __x ->
        Lwt_io.write_value __x protocol)
end

module Trusted_validators_membership_change = struct
  type t = Trusted_validators_membership_change.t [@@deriving yojson]

  let read = read_json [%of_yojson: t list]

  let write = write_json [%to_yojson: t list]
end
