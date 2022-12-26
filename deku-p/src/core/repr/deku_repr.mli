module Prefix : sig
  type prefix
  type t = prefix

  val contract_hash : prefix
  val block_hash : prefix
  val operation_hash : prefix
  val protocol_hash : prefix
  val ed25519_public_key_hash : prefix
  val secp256k1_public_key_hash : prefix
  val p256_public_key_hash : prefix
  val ed25519_public_key : prefix
  val secp256k1_public_key : prefix
  val p256_public_key : prefix
  val ed25519_seed : prefix
  val secp256k1_secret_key : prefix
  val p256_secret_key : prefix
  val ed25519_signature : prefix
  val secp256k1_signature : prefix
  val p256_signature : prefix
  val chain_id : prefix
  val deku_contract_hash : prefix
  val deku_block_hash : prefix
  val deku_operation_hash : prefix
  val deku_handshake_challenge_hash : prefix
  val deku_message_hash : prefix
  val deku_request_hash : prefix
  val deku_withdrawal_hash : prefix
end

module With_b58 (P : sig
  type t

  val prefix : Prefix.t
  val to_raw : t -> string
  val of_raw : string -> t option
end) : sig
  val of_b58 : string -> P.t option
  val to_b58 : P.t -> string
end

module With_b58_and_encoding (P : sig
  type t

  val name : string
  val prefix : Prefix.t
  val size : int
  val to_raw : t -> string
  val of_raw : string -> t option
end) : sig
  val encoding : P.t Data_encoding.t
  val of_b58 : string -> P.t option
  val to_b58 : P.t -> string
end

val decode_variant : (string -> 'a option) list -> string -> 'a option

val make_encoding :
  name:string ->
  to_string:('a -> string) ->
  of_string:(string -> 'a option) ->
  raw_encoding:'a Data_encoding.t ->
  'a Data_encoding.t

module With_encoding (P : sig
  type t

  val name : string
  val prefix : Prefix.t
  val size : int
  val to_raw : t -> string
  val of_raw : string -> t option
end) : sig
  val encoding : P.t Data_encoding.t
end
