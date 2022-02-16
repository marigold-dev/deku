module type Entry = sig
  type t
end

module type Container = sig
  type pointer = int

  type entry

  type t

  val pointer : t -> pointer

  val blit : entry array -> t -> int

  val set_pointer : t -> value:pointer -> unit

  val read_and_increment : t -> entry

  val make : default_elem:entry -> cap:pointer -> unit -> t

  val of_list : entry list -> t -> unit

  val memory : t -> entry array

  val read : t -> pointer:pointer -> entry

  val write : t -> pointer:pointer -> data:entry -> unit

  val push : t -> data:entry -> unit

  val pop : t -> entry

  val tail : t -> unit

  val read_offset : t -> offset:pointer -> entry
end

module Make (E : Entry) : Container with type entry := E.t

module Env : Container with type entry := int

module Stack : Container with type entry := int

module Memory : sig
  include Container with type entry := int

  val allocate : t -> size:pointer -> tag:pointer -> pointer

  val read_field : t -> block:pointer -> field:pointer -> pointer

  val write_field : t -> block:pointer -> field:pointer -> value:pointer -> unit
end
