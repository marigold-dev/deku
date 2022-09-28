module Prefix : sig
  val contract_hash : string
  val block_hash : string
  val operation_hash : string
  val protocol_hash : string
  val ed25519_public_key_hash : string
  val secp256k1_public_key_hash : string
  val p256_public_key_hash : string
  val ed25519_public_key : string
  val secp256k1_public_key : string
  val p256_public_key : string
  val ed25519_seed : string
  val secp256k1_secret_key : string
  val p256_secret_key : string
  val ed25519_signature : string
  val secp256k1_signature : string
  val p256_signature : string
  val chain_id : string
  val deku_contract_hash : string
  val deku_block_hash : string
  val deku_operation_hash : string
  val deku_message_hash : string
  val deku_request_hash : string
end

module With_b58 (P : sig
  type t

  val prefix : string
  val to_raw : t -> string
  val of_raw : string -> t option
end) : sig
  val of_b58 : string -> P.t option
  val to_b58 : P.t -> string
end

(* yojson exceptions *)
exception Not_a_string
exception Not_a_b58

module With_yojson_of_b58 (P : sig
  type t

  val of_b58 : string -> t option
  val to_b58 : t -> string
end) : sig
  val t_of_yojson : [> `String of string ] -> P.t
  val yojson_of_t : P.t -> [> `String of string ]
end

module With_b58_and_yojson (P : sig
  type t

  val prefix : string
  val to_raw : t -> string
  val of_raw : string -> t option
end) : sig
  val of_b58 : string -> P.t option
  val to_b58 : P.t -> string
  val t_of_yojson : [> `String of string ] -> P.t
  val yojson_of_t : P.t -> [> `String of string ]
end

val decode_variant : (string -> 'a option) list -> string -> 'a option
