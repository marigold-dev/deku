module type Sized = sig
  type t

  include Bin_prot.Binable.S with type t := t
end

module type Gas_counter = sig
  exception Gas_limit_exceeded

  type t

  type entry

  (** creates a new gas counter *)
  val make : unit -> t

  (** reuses gas counter, not sure if it's useful but let it be here *)
  val reuse : t -> t

  val sized : t -> item:entry -> t

  val simple : t -> t

  val simple_n : t -> int -> t
end

module Dummy_Gas (S : Sized) : Gas_counter with type entry := S.t = struct
  exception Gas_limit_exceeded

  type t = int

  let make () = max_int

  let reuse r = r

  let[@inline.always] sized t ~item = t - S.bin_size_t item

  let[@inline.always] simple t = pred t

  let[@inline.always] simple_n t amount = t - amount
end
