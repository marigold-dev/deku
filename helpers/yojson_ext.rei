let with_yojson_string:
  (string => result('a, string), 'b => string) =>
  (Yojson.Safe.t => result('a, string), 'b => Yojson.Safe.t);
