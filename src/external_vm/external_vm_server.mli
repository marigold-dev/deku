val start_chain_ipc : named_pipe_path:string -> unit

val main :
  External_vm_protocol.set list ->
  (Crypto.Key_hash.t ->
  Crypto.BLAKE2B.t ->
  Yojson.Safe.t ->
  (unit, string) result) ->
  unit
