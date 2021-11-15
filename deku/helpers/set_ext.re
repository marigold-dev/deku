include Set;
module Make_with_yojson =
       (
         V: {
           include Set.OrderedType;
           let to_yojson: t => Yojson.Safe.t;
           let of_yojson: Yojson.Safe.t => result(t, string);
         },
       ) => {
  include Set.Make(V);
  let to_yojson = t => t |> to_seq |> List.of_seq |> [%to_yojson: list(V.t)];
  let of_yojson = json =>
    json |> [%of_yojson: list(V.t)] |> Result.map(of_list);
};
