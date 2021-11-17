type t =
  | Forall
  | Forall_TC
  | Builtin_type
  | Propagator_break_ctor of string
  | Propagator_access_label of string
  | Propagator_specialize_apply
  | Propagator_specialize_tf
  | Propagator_specialize_targ
  | Propagator_specialize_eq
  | Todo of string

val pp : Format.formatter -> t -> unit
val wrap : t -> 'v -> 'v Location.wrap
