open Deku_concepts

type included_operation_set
type t = included_operation_set

val empty : included_operation_set

val add :
  Operation.Initial.t -> included_operation_set -> included_operation_set

val mem : Operation.Initial.t -> included_operation_set -> bool

val drop :
  current_level:Level.t -> included_operation_set -> included_operation_set

val encoding : included_operation_set Data_encoding.t
