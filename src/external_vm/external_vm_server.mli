val start_chain_ipc : named_pipe_path:string -> unit

type storage = {
  set : string -> Yojson.Safe.t -> unit;
  get : string -> Yojson.Safe.t option;
}

val main :
  External_vm_protocol.set list ->
  (storage ->
  Crypto.Key_hash.t ->
  Crypto.BLAKE2B.t ->
  Yojson.Safe.t ->
  (unit, string) result) ->
  unit
