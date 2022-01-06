module type Executor = sig
  type address

  type contract

  type hash

  type chain_id

  type key

  type key_hash

  val get_contract_opt : address -> contract option

  val chain_id : chain_id

  val key_hash : key -> key_hash
end
