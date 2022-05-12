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
  open Contract_context

  val invoke :
    Contract.t ->
    ctx:
      < CTX.State.table_access
      ; CTX.State.addressing
      ; CTX.State.with_operations
      ; .. > ->
    arg:Invocation_payload.t ->
    gas:int ->
    (* TODO: unit should be user operation list *)
    (Contract.t * int list, string) result
end
