(* Purely functional queues *)

type 'a t

val empty    : 'a t
val enq      : 'a -> 'a t -> 'a t
val deq      : 'a t -> ('a t * 'a) option

val is_empty : 'a t -> bool

(* The call [peek q] is [None] if the queue [q] is empty, and,
   otherwise, is a pair made of a queue and the next item in it to be
   dequeued. The returned queue contains the same items as [q], in the
   same order, but more efficient, in general, to use in further
   calls. *)

val peek     : 'a t -> ('a t * 'a) option
