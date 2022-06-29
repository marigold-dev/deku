module Errors : sig
  type t =
    [ `Initialization_error
    | `Module_validation_error
    | `Execution_error ]
  [@@deriving show]
end

module Memory : sig
  type t

  val load : t -> address:int64 -> int

  val store_bytes : t -> address:int64 -> content:bytes -> unit

  val load_bytes : t -> address:int64 -> size:int -> bytes
end

module Module : sig
  type t [@@deriving yojson]

  val of_string : code:string -> (t, Errors.t) result

  val encode : t -> (string, string) result

  val decode : string -> (t, Errors.t) result
end

module Runtime : sig
  val invoke :
    (Memory.t -> int64 -> unit) ->
    module_:Module.t ->
    gas:int ref ->
    argument:bytes ->
    storage:bytes ->
    (bytes * int list, Errors.t) result
  (** [invoke syscall_fn module_ runtime gas argument storage] invokes the entrypoint of the contract
        with a given syscall, argument and storage. *)
end
