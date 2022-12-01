val compile_contract :
  env:Eio.Stdenv.t ->
  lang:string ->
  filename_ligo:string ->
  filename_tz:string ->
  unit ->
  unit

val compile_parameter :
  lang:string -> filename_ligo:string -> expression:string -> unit -> string

val compile_storage :
  lang:string -> filename_ligo:string -> expression:string -> unit -> string
