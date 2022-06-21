val get_initial_state :
  folder:string ->
  pollinate_node_opt:Pollinate.PNode.t ref Lwt.t option ->
  Node.State.t Lwt.t
