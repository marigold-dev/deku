open Tezos
open Crypto
module Raw : sig
  module Script : sig
    type t [@@deriving yojson]
    val make : string -> (t, string) result
  end
  module Value : sig
    type t [@@deriving yojson]
    val make : string -> (t, string) result
  end
end

module Origination_payload : sig
  type t [@@deriving yojson]
  val pp : Format.formatter -> t -> unit
  val make_lambda :
    code:Raw.Script.t -> storage:Raw.Value.t -> (t, string) result
end

module Contract : sig
  type t [@@deriving yojson]
  val to_string : t -> string
  val originated_by : t -> Key_hash.t
  module Compile : sig
    val compile_script :
      Origination_payload.t ->
      gas:int ->
      on_error:(string -> int -> 'a) ->
      originated_by:Key_hash.t ->
      (t * int, 'a) result
  end
end

module Contract_storage : sig
  type t [@@deriving yojson]

  val empty : t
  val originate_contract :
    t -> address:Contract_hash.t -> contract:Contract.t -> t
  val update_contract_storage :
    t -> address:Contract_hash.t -> updated_contract:Contract.t -> t
  val get_contract : t -> address:Contract_hash.t -> Contract.t option
end
