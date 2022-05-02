open Node
open State
exception Invalid_json of string
module Identity : sig
  val read : file:string -> identity Lwt.t
  val write : identity -> file:string -> unit Lwt.t
end
module Wallet : sig
  type t = {
    address : Crypto.Key_hash.t;
    priv_key : Crypto.Secret.t;
  }
  val read : file:string -> t Lwt.t
  val write : t -> file:string -> unit Lwt.t
end
module Interop_context : sig
  type t = {
    rpc_node : Uri.t;
    secret : Crypto.Secret.t;
    consensus_contract : Tezos.Address.t;
    discovery_contract : Tezos.Address.t;
    required_confirmations : int;
  }
  val read : file:string -> t Lwt.t
  val write : t -> file:string -> unit Lwt.t
end
module State_bin : sig
  val read : file:string -> Protocol.t Lwt.t
  val write : Protocol.t -> file:string -> unit Lwt.t
end
module Trusted_validators_membership_change : sig
  type t = Trusted_validators_membership_change.t [@@deriving yojson]
  val read : file:string -> t list Lwt.t
  val write : t list -> file:string -> unit Lwt.t
end
