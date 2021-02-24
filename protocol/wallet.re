[@deriving (ord, yojson)]
type t = Address.t;

let of_address = t => t;
let get_address = t => t;

module Map = {
  include Map.Make({
    type t = Address.t;
    let compare = compare;
  });
  let to_yojson = (f, t) =>
    t |> to_seq |> List.of_seq |> [%to_yojson: list((t, 'a))](f);
  let of_yojson = (f, json) =>
    json
    |> [%of_yojson: list((t, 'a))](f)
    |> Result.map(list => list |> List.to_seq |> of_seq);
};
