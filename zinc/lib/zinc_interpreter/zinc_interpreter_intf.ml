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
end
