module type MAIN = sig
  type t
  val process_message : Message.t -> t -> t * Message.t list

  val tick : Time.Timestep.t -> t -> t * Message.t list

  val filter_msgs : Message.t list -> Message.t list

  val dump : t -> unit Lwt.t

  val load : file:string -> t Lwt.t
end
