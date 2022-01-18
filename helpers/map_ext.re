include Map;
module Make_with_yojson =
       (
         K: {
           include Map.OrderedType;
           let to_yojson: t => Yojson.Safe.t;
           let of_yojson: Yojson.Safe.t => result(t, string);
         },
       ) => {
  include Map.Make(K);

  let to_yojson = (f, t) => {
    t |> bindings |> [%to_yojson: list((K.t, 'a))](f);
  };
  let of_yojson = (f, json) =>
    json
    |> [%of_yojson: list((K.t, 'a))](f)
    |> Result.map(l => l |> List.to_seq |> of_seq);

  let update_entry = (key, f, m) => {
    let value = find_opt(key, m);
    switch (f(value)) {
    | Ok((new_value, data)) => Ok((add(key, new_value, m), data))
    | Error(_) as e => e
    };
  };

  let key_to_yojson = K.to_yojson;
  let key_of_yojson = K.of_yojson;
};
