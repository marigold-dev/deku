module Contract : sig
  type t [@@deriving yojson, eq, bin_io]
end

module Origination_payload : sig
  type t [@@deriving yojson, bin_io]

  val lambda_of_yojson :
    code:Yojson.Safe.t -> storage:Yojson.Safe.t -> (t, string) result

  val dummy_of_yojson : storage:int -> t
end

module Compiler : sig
  val compile : Origination_payload.t -> gas:int -> (Contract.t, string) result
end

module Invocation_payload : sig
  type t [@@deriving yojson, bin_io]

  val lambda_of_yojson : arg:Yojson.Safe.t -> (t, string) result

  val dummy_of_yojson : arg:Yojson.Safe.t -> (t, string) result
end

module Interpreter : sig
  val invoke :
    Contract.t ->
    source:Crypto.Key_hash.t ->
    arg:Invocation_payload.t ->
    gas:int ->
    (* TODO: unit should be user operation list *)
    (Contract.t * unit, string) result
end
