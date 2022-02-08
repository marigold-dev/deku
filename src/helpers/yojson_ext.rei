val with_yojson_string :
  string ->
    ('a -> string) ->
      (string -> 'a option) ->
        (('a -> Yojson.Safe.t) * (Yojson.Safe.t -> ('a, string) result))