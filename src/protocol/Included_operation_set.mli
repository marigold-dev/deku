open Deku_concepts

type included_operation_set
type t = included_operation_set

val empty : included_operation_set
val add : Operation.t -> included_operation_set -> included_operation_set
val mem : Operation.t -> included_operation_set -> bool
val drop : current:Level.t -> included_operation_set -> included_operation_set
