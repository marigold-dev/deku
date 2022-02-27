val with_yojson_string :
  string ->
  ('a -> string) ->
  (string -> 'a option) ->
  ('a -> Yojson.Safe.t) * (Yojson.Safe.t -> ('a, string) result)

val with_data_encoding :
  'a Data_encoding.t ->
  ('a -> Yojson.Safe.t) * (Yojson.Safe.t -> ('a, string) result)
