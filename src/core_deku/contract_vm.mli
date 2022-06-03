module Contract : sig
  type t [@@deriving yojson, eq]
end
module Origination_payload : sig
  type t [@@deriving yojson]

  val lambda_of_yojson :
    code:Yojson.Safe.t -> storage:Yojson.Safe.t -> (t, string) result
  val dummy_of_yojson : storage:int -> t
  val wasm_of_yojson : code:bytes -> storage:bytes -> (t, string) result
end
module Compiler : sig
  val compile :
    Origination_payload.t ->
    gas:int ->
    tickets:(string * string) list ->
    (Contract.t, string) result
end

module Invocation_payload : sig
  type t [@@deriving yojson]

  val lambda_of_yojson : arg:Yojson.Safe.t -> (t, string) result
  val dummy_of_yojson : arg:Yojson.Safe.t -> (t, string) result
  val wasm_of_yojson : arg:Yojson.Safe.t -> (t, string) result
  val of_bytes : arg:bytes -> (t, string) result
end

module Interpreter : sig
  val invoke :
    Contract.t ->
    ctx:(module Contract_context.CTX) ->
    arg:Invocation_payload.t ->
    gas:int ->
    (* TODO: unit should be user operation list *)
    (Contract.t * Contract_context.Contract_operation.t list, string) result
end
