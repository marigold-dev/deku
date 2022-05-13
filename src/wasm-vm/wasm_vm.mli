module Contract : sig
  type t

  val make : storage:bytes -> code:string -> (t, string) result
  (** [make ~storage ~code] creates new contract instance with [storage] and [code]. 
        [storage] can be the initial storage of the updated one
        and [code] is a WebAssembly Text Format for now. *)
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

  type address = int64

  val blit : t -> address -> bytes -> unit

  val sub : t -> address -> int -> bytes
end

module Extern : sig
  type t = Func of func_type * (Memory.t -> Value.t list -> Value.t option)
  and func_type = value_type list * value_type option
  and value_type =
    | I32
    | I64

  val func : func_type -> (Memory.t -> Value.t list -> Value.t option) -> t
end

module Runtime : sig
  type t

  val make : contract:Contract.t -> imports:Extern.t list -> int ref -> t
  (** [make contract gas] instantiates a new contract runtime to be invoked. *)

  val invoke : t -> int ref -> bytes -> (bytes, string) result
  (** [invoke runtime gas argument] invokes the entrypoint of the contract
        with a given argument.
        
        The contract will receive two arguments: a pointer to the argument
        in memory and a pointer to the storage. The contract should return
        the size of the new storage that should be on the same place as before. *)
end
