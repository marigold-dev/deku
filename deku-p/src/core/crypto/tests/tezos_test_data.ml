module type Tezos_data = sig
  val public_keys : string list
  val compared_secret_keys : string list
  val equality_secret_keys : bool
  val secret_key_encoding : int list
  val compared_public_keys : string list
  val equality_public_keys : bool
  val public_key_encoding : int list
  val compared_key_hashes : string list
  val equality_key_hash : bool
  val public_key_hash_encoding : int list
  val to_sign : string
  val signatures : string
  val verified_normal_signatures : bool list list
  val verified_after_conversion : bool list list
  val all_verified_normal : bool
  val all_verified_post_conversion : bool
  val compare_signatures : string
  val equality_signatures : bool
  val signature_encoding : int list
  val zero : string
  val size : int
end
