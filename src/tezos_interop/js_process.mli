(** Interface between a JS script and an OCaml consumer *)

type js_process
type t = js_process

val spawn : file:string -> (js_process -> unit) -> 'a

val listen :
  js_process -> Yojson.Safe.t -> on_message:(Yojson.Safe.t -> unit) -> unit

val request : js_process -> Yojson.Safe.t -> Yojson.Safe.t
