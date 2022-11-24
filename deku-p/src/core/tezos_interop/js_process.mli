(** Interface between a JS script and an OCaml consumer *)

type js_process
type t = js_process

val spawn : file:string -> (js_process -> unit) -> 'a

val listen :
  js_process ->
  Data_encoding.Json.t ->
  on_message:(Data_encoding.Json.t -> unit) ->
  unit

val request : js_process -> Data_encoding.Json.t -> Data_encoding.Json.t
