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
module Ffi : sig
  module type CTX = sig
    module Address : sig
      type t
      val size : int
      val of_bytes : bytes -> t
      val to_bytes : t -> bytes
    end
    module Ticket_handle : sig
      type t
      val size : int
      val of_bytes : bytes -> t
      val to_bytes : t -> bytes
    end
    module Ticket_id : sig
      type t
      val size : t -> int
      val to_bytes : t -> bytes
    end
    module Amount : sig
      type t
      val size : int
      val of_int : int -> t
      val to_int : t -> int
    end
    val get_source : Address.t
    val sender : unit -> Address.t
    val source : unit -> Address.t
    val self : unit -> Address.t
    val read_ticket :
      Ticket_handle.t -> Ticket_id.t * Amount.t * Ticket_handle.t
    val split_ticket :
      Ticket_handle.t * Amount.t * Amount.t -> Ticket_handle.t * Ticket_handle.t
    val join_tickets : Ticket_handle.t * Ticket_handle.t -> Ticket_handle.t
    val own_ticket : Ticket_handle.t -> Ticket_handle.t
    val get_contract_opt : Address.t -> Address.t option
    val transaction : bytes * (Ticket_handle.t * Amount.t) * Address.t -> int
  end
  module Make (C : CTX) : sig
    val custom : Memory.t -> int64 -> unit
  end
end
