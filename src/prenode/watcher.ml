module type MAIN = sig
  type t
  val process_message : Message.t -> t -> t * Message.t list

  val tick : Time.Timestep.t -> t -> t * Message.t list
end
