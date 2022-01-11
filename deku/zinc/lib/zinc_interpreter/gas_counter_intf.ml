module type Gas_counter = sig
  exception Gas_limit_exceeded

  type t

  (** creates a new gas counter *)
  val make : unit -> t

  (** reuses gas counter, not sure if it's useful but let it be here *)
  val reuse : t -> t

  val sized : t -> item:'a -> sizer:('a -> int) -> unit

  val simple : t -> unit
end
