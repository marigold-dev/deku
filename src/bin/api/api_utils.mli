val input_of_body :
  of_yojson:(Yojson.Safe.t -> 'a) ->
  Dream.request ->
  ('a, Api_error.t) result Lwt.t

