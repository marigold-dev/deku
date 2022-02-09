open Crypto
module Consensus : sig
  type t =
    | Add_validator    of Validators.validator
    | Remove_validator of Validators.validator
  [@@deriving eq, ord, yojson]
  val sign : Secret.t -> t -> Signature.t
  val verify : Key.t -> Signature.t -> t -> bool
end
module Core_tezos : sig
  type t = Core.Tezos_operation.t [@@deriving eq, ord, yojson]
end
module Core_user : sig
  type t = private {
    hash : BLAKE2B.t;
    key : Key.t;
    signature : Signature.t;
    nonce : int32;
    block_height : int64;
    data : Core.User_operation.t;
  }
  [@@deriving eq, ord, yojson]
  val sign :
    secret:Secret.t ->
    nonce:int32 ->
    block_height:int64 ->
    data:Core.User_operation.t ->
    t
  val unsafe_make :
    hash:BLAKE2B.t ->
    key:Key.t ->
    signature:Signature.t ->
    nonce:int32 ->
    block_height:int64 ->
    data:Core.User_operation.t ->
    t
end
type t =
  | Core_tezos of Core.Tezos_operation.t
  | Core_user  of Core_user.t
  | Consensus  of Consensus.t
[@@deriving eq, ord, yojson]
