val file_exists : string -> bool

val spawn :
  prog:string ->
  args:string array ->
  (stdin:Eio.Flow.sink -> stdout:Eio.Flow.source -> 'a) ->
  'a
