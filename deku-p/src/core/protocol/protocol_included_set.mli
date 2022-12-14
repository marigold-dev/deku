open Deku_concepts

type included_set
type t = included_set

val empty : included_set
val add : Protocol_operation.Raw.t -> included_set -> included_set
val mem : Protocol_operation.Raw.t -> included_set -> bool
val drop : current_level:Level.t -> included_set -> included_set
val encoding : included_set Data_encoding.t
