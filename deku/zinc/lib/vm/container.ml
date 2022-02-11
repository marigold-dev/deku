module type Entry = sig
  type t
end

module type Container = sig
  type pointer = (int[@unboxed])

  type entry

  type t

  val pointer : t -> pointer

  val blit : entry array -> t -> unit

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

module Make (E : Entry) : sig
  type pointer = int

  type entry = E.t

  type t = {mutable pointer : pointer; memory : entry array}

  val pointer : t -> pointer

  val set_pointer : t -> value:pointer -> unit

  val read_and_increment : t -> entry

  val blit : entry array -> t -> unit

  val make : default_elem:entry -> cap:pointer -> unit -> t

  val of_list : entry list -> t -> unit

  val memory : t -> entry array

  val read : t -> pointer:pointer -> entry

  val write : t -> pointer:pointer -> data:entry -> unit

  val push : t -> data:entry -> unit

  val pop : t -> entry

  val tail : t -> unit

  val read_offset : t -> offset:pointer -> entry
end = struct
  type pointer = int

  type entry = E.t

  type t = {mutable pointer : int; memory : E.t array}

  let[@inline always] blit code t =
    Array.blit code 0 t.memory 0 (Array.length code)

  let make ~default_elem ~cap () =
    {pointer = 0; memory = Array.make cap default_elem}

  let[@inline always] pointer t = t.pointer

  let[@inline always] set_pointer t ~value = t.pointer <- value

  let of_list l t = List.iteri (fun idx x -> Array.set t.memory idx x) l

  let[@inline always] memory t = t.memory

  let[@inline always] read_and_increment t =
    let res = Array.get t.memory t.pointer in
    t.pointer <- succ t.pointer ;
    res

  let[@inline always] read_offset t ~offset =
    let offset = t.pointer - offset in
    Array.get t.memory offset

  let[@inline always] read (t : t) ~pointer = Array.get t.memory pointer

  let[@inlined] write (t : t) ~pointer ~data = Array.set t.memory pointer data

  let[@inline always] tail ({pointer; _} as t) = t.pointer <- pred pointer

  let[@inline always] push (t : t) ~data =
    let pointer = t.pointer in
    Array.set t.memory pointer data ;
    t.pointer <- succ pointer

  let[@inline always] pop (t : t) =
    let pointer = pred t.pointer in
    let value = Array.get t.memory pointer in
    t.pointer <- pointer ;
    value
end

module Env : Container with type entry := int = struct
  include Make (struct
    type t = int
  end)
end

module Stack : Container with type entry := int = struct
  include Make (struct
    type t = int
  end)
end

module Memory : sig
  include Container with type entry := int

  val allocate : t -> size:pointer -> tag:pointer -> pointer

  val read_field : t -> block:pointer -> field:pointer -> int

  val write_field : t -> block:pointer -> field:pointer -> value:int -> unit
end = struct
  include Make (struct
    type t = int
  end)

  let[@inline always] allocate (t : t) ~size ~tag =
    let point = t.pointer in
    Array.set t.memory point size ;
    Array.set t.memory (point + 1) tag ;
    t.pointer <- point + (size + 3) ;
    point

  let[@inline always] read_field (t : t) ~block ~field =
    Array.get t.memory (block + field + 1)

  let[@inline always] write_field (t : t) ~block ~field ~value =
    Array.set t.memory (block + field + 1) value
end
