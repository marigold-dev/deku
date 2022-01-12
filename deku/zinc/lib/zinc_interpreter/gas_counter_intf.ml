module type Sized = sig
  type t

  val size : t -> int
end

module type Gas_counter = sig
  exception Gas_limit_exceeded

  type t

  type entry

  (** creates a new gas counter *)
  val make : unit -> t

  (** reuses gas counter, not sure if it's useful but let it be here *)
  val reuse : t -> t

  val sized : t -> item:entry -> unit

  val simple : t -> unit
end

module Dummy_Gas (S : Sized) : Gas_counter with type entry := S.t = struct
  exception Gas_limit_exceeded

  type t = int ref

  let make () = ref max_int

  let reuse r = ref !r

  let sized t ~item =
    let amount = S.size item in
    t := !t - amount

  let simple t = decr t
end
