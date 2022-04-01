type t
val of_yojson : Yojson.Safe.t -> (t, string) result
val to_yojson : t -> Yojson.Safe.t
val empty : t
val start_vm : path:string -> named_pipe_path:string -> unit
val apply_vm_operation : t -> Yojson.Safe.t -> t
