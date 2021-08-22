let with_yojson_string = (from, to_) => (
  (json: Yojson.Safe.t) =>
    [%of_yojson: string](json) |> Result.bind(_, from),
  (t) => (`String(to_(t)): Yojson.Safe.t),
);
