let with_yojson_string:
  (string, 'a => string, string => option('a)) =>
  ('a => Yojson.Safe.t, Yojson.Safe.t => result('a, string));
