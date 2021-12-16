(* REMOVE THIS STUFF WHEN WE WILL GET ZINC IN *)
module type With_string = sig
  type t

  val of_string : string -> t option

  val to_string : t -> string
end

module type With_yojson = sig
  type t

  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or

  val to_yojson : t -> Yojson.Safe.t
end

module type With_eq = sig
  type t

  val equal : t -> t -> bool
end

module type With_domain_derivation = sig
  type t

  include With_string with type t := t

  include With_yojson with type t := t

  include With_eq with type t := t
end

module type Executor = sig
  module Address : sig
    (*Tezos.Address*)
    type t

    include With_domain_derivation with type t := t
  end

  module Contract : sig
    (* Tezos.Contract - TODO *)
    type t

    include With_domain_derivation with type t := t

    val get_contract_opt : Address.t -> t option
  end

  module Chain_id : sig
    (* Tezos.Contract - TODO *)
    type t

    include With_domain_derivation with type t := t

    val chain_id : t
  end

  module Hash : sig
    (*Crypto.BLAKE2B*)
    type t

    include With_domain_derivation with type t := t

    val hash : string -> t
  end

  module Key : sig
    (*Crypto.Key*)
    type t

    val hash_key : t -> Hash.t

    include With_domain_derivation with type t := t
  end
end

module Executor: Executor = struct 
  module Key = struct 
    include Crypto.Key
    let hash_key t = to_string t |> Crypto.BLAKE2B.hash
  end
  module Address = struct
    include Tezos.Address
  end
  module Hash = struct
    include Crypto.BLAKE2B
  end
  module Chain_id = struct 
    (*WHAT???*)
    include Crypto.BLAKE2B
    let chain_id = Crypto.BLAKE2B.hash "TODO" 
  end
  module Contract = Protocol.Contract
end
