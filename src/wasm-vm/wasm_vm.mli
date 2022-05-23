module Errors : sig
  type t =
    [ `Initialization_error
    | `Module_validation_error
    | `Execution_error ]
  [@@deriving show]
end

module Value : sig
  type t

  val i32 : int32 -> t

  val i64 : int64 -> t

  val f32 : float -> t

  val f64 : float -> t

  val to_int32 : t -> int32 option

  val to_int64 : t -> int64 option

  val to_f32 : t -> float option

  val to_f64 : t -> float option
end

module Memory : sig
  type t
  val load : t -> address:int64 -> int
  val store_bytes : t -> address:int64 -> content:bytes -> unit
  val load_bytes : t -> address:int64 -> size:int -> bytes
end

module Module : sig
  type t
  val of_string : gas:int ref -> code:string -> (t, Errors.t) result
  val encode : t -> (string, string) result
  val decode : string -> (t, string) result
end

module Runtime : sig
  val invoke :
    (Memory.t -> int64 -> unit) ->
    module_:Module.t ->
    gas:int ref ->
    argument:bytes ->
    storage:bytes ->
    (bytes, Errors.t) result
  (** [invoke runtime gas argument] invokes the entrypoint of the contract
        with a given argument.

        The contract will receive two arguments: a pointer to the argument
        in memory and a pointer to the storage. The contract should return
        the size of the new storage that should be on the same place as before. *)
end
