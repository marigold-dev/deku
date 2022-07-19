open Helpers
open Crypto

module Identity : sig
  val read : file:string -> Consensus.identity Lwt.t

  val write : Consensus.identity -> file:string -> unit Lwt.t
end

module Wallet : sig
  type t = {
    address : Crypto.Key_hash.t;
    priv_key : Crypto.Secret.t;
  }

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) result

  val read : file:string -> t Lwt.t

  val write : t -> file:string -> unit Lwt.t
end

module Interop_context : sig
  type t = {
    rpc_node : Uri.t;
    secret : Secret.t;
    consensus_contract : Tezos.Address.t;
    discovery_contract : Tezos.Address.t;
    required_confirmations : int;
  }

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or

  val read : file:string -> t Lwt.t

  val write : t -> file:string -> unit Lwt.t
end

module State_bin : sig
  val read : file:string -> Protocol.t Lwt.t

  val write : Protocol.t -> file:string -> unit Lwt.t
end

module Trusted_validators_membership_change : sig
  type t = Consensus.Trusted_validators_membership_change.t

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or

  val read : file:string -> t list Lwt.t

  val write : t list -> file:string -> unit Lwt.t
end
