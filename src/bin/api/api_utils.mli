val input_of_body :
  of_yojson:(Yojson.Safe.t -> 'a) ->
  Dream.request ->
  ('a, Api_error.t) result Lwt.t

val param_of_request : Dream.request -> string -> string option
