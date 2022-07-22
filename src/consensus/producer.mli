open Deku_concepts

type producer
type t = producer

val make : identity:Identity.t -> producer

val produce :
  current:Timestamp.t ->
  level:Level.t ->
  previous:Block_hash.t ->
  producer ->
  Block.t * producer
