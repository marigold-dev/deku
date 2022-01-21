module type Executor = sig
  type address

  type hash

  type chain_id

  type ticket

  type key

  type key_hash

  type contract

  val get_contract_opt : address -> contract option

  val chain_id : chain_id

  val key_hash : key -> key_hash

  module Pack : sig
    type t

    type result =
      | Int of Z.t
      | String of string
      | Bytes of bytes
      | List of result list
      | Error of string

    val int : Z.t -> t

    val string : string -> t

    val bytes : bytes -> t

    val list : t list -> t

    val key : key -> t

    val key_hash : key_hash -> t

    val address : address -> t

    val to_bytes : t -> bytes

    val of_bytes : bytes -> result
  end
end
