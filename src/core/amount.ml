type t = int [@@deriving eq, ord]

let zero = 0

let ( + ) = ( + )

let ( - ) a b =
  let t = a - b in
  if t < 0 then raise (Invalid_argument "Negative amount");
  t

let of_int t =
  if t < 0 then raise (Invalid_argument "Negative amount");
  t

let to_int t = t

let of_yojson json = json |> [%of_yojson: int] |> Result.map of_int

let to_yojson t = `Int t
