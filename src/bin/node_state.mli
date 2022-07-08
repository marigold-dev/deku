val get_initial_state :
  folder:string ->
  minimum_block_delay:float ->
  pollinate_context:Node.State.pollinate_context ->
  Node.State.t Lwt.t

