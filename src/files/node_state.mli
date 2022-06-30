val get_initial_consensus_state : folder:string -> Consensus.state Lwt.t

val get_initial_state :
  folder:string -> minimum_block_delay:float -> Node.State.t Lwt.t
