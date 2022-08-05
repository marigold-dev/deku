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
  val deku_message_hash : prefix
end

(* yojson exceptions *)
exception Not_a_b58

module With_encodings (P : sig
  type t

  val prefix : Prefix.t
  val to_raw : t -> string
  val of_raw : string -> t option
end) : sig
  val of_b58 : string -> P.t option
  val to_b58 : P.t -> string
  val t_of_yojson : Yojson.Safe.t -> P.t
  val yojson_of_t : P.t -> Yojson.Safe.t
end

module With_all_encodings (P : sig
  type t

  val name : string
  val title : string
  val prefix : Prefix.t
  val size : int
  val to_raw : t -> string
  val of_raw : string -> t option
end) : sig
  val encoding : P.t Data_encoding.t
  val of_b58 : string -> P.t option
  val to_b58 : P.t -> string
  val t_of_yojson : Yojson.Safe.t -> P.t
  val yojson_of_t : P.t -> Yojson.Safe.t
end

module With_encodings_of_many (P : sig
  type t

  val of_b58 : (string -> t option) list
  val to_b58 : t -> string
end) : sig
  val of_b58 : string -> P.t option
  val to_b58 : P.t -> string
  val t_of_yojson : Yojson.Safe.t -> P.t
  val yojson_of_t : P.t -> Yojson.Safe.t
end

module With_all_encodings_of_many (P : sig
  type t

  val key : string
  val name : string
  val title : string
  val cases : t Data_encoding.case list
  val of_b58 : (string -> t option) list
  val to_b58 : t -> string
end) : sig
  val encoding : P.t Data_encoding.t
  val of_b58 : string -> P.t option
  val to_b58 : P.t -> string
  val t_of_yojson : Yojson.Safe.t -> P.t
  val yojson_of_t : P.t -> Yojson.Safe.t
end

(* TODO: this should probably not be exposed *)

module With_encoding_of_b58 (P : sig
  type t

  val name : string
  val title : string
  val raw_encoding : t Data_encoding.t
  val of_b58 : string -> t option
  val to_b58 : t -> string
end) : sig
  val encoding : P.t Data_encoding.t
end
