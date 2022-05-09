open Crypto

type t
val of_yojson : Yojson.Safe.t -> (t, string) result
val to_yojson : t -> Yojson.Safe.t
val empty : t
val start_vm_ipc : named_pipe_path:string -> unit
val apply_vm_operation :
  state:t -> source:Key_hash.t -> tx_hash:BLAKE2B.t -> Yojson.Safe.t -> t
val close_vm_ipc : unit -> unit
