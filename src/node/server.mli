val start : initial:State.t -> unit
val get_state : unit -> State.t
val set_state : State.t -> unit
val get_port : unit -> int option
val get_consensus : unit -> Tendermint.t
val set_consensus : Tendermint.t -> unit
