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

let pp : Format.formatter -> t -> unit = fun ppf r ->
  match r with
  | Forall -> Format.fprintf ppf "forall"
  | Forall_TC -> Format.fprintf ppf "forall_tc"
  | Builtin_type -> Format.fprintf ppf "built-in type"
  | Todo i -> Format.fprintf ppf "?TODO%s?" i
  | Propagator_break_ctor id -> Format.fprintf ppf "propagator break_ctor %s" id
  | Propagator_access_label id -> Format.fprintf ppf "propagator break_ctor %s" id
  | Propagator_specialize_apply -> Format.fprintf ppf "propagator specialize apply"
  | Propagator_specialize_tf -> Format.fprintf ppf "propagator specialize tf"
  | Propagator_specialize_targ -> Format.fprintf ppf "propagator specialize targ"
  | Propagator_specialize_eq -> Format.fprintf ppf "propagator specialize equation"

let wrap : t -> 'v -> 'v Location.wrap = fun reason v ->
  let loc = Location.virtual_location @@ Format.asprintf "%a" pp reason in
  Location.wrap ~loc @@ v