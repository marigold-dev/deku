// option
let some = Option.some;
let (let.none) = (v, f) =>
  switch (v) {
  | None => f()
  | Some(v) => Some(v)
  };
let (let.some) = Option.bind;
let (let.default) = (v, f) => Option.value(f(), ~default=v);

// result
let ok = Result.ok;
let (let.ok) = Result.bind;
let (let.assert) = ((message, bool), f) => bool ? f() : Error(message);
let rec fold_left_ok = (f, state) =>
  fun
  | [] => Ok(state)
  | [head, ...tl] =>
    switch (f(state, head)) {
    | Ok(state) => fold_left_ok(f, state, tl)
    | Error(error) => Error(error)
    };

// lwt

let await = Lwt.return;
let (let.await) = Lwt.bind;

// TODO: this concurrency number yeah don't like it
let list_p_limited_concurrency = (~concurrency=20, op, f) => {
  let queue = Lwt_pool.create(concurrency, () => Lwt.return());
  op(x => Lwt_pool.use(queue, () => f(x)));
};
let filter_p_limited_concurrency = (~concurrency=20, f) =>
  list_p_limited_concurrency(~concurrency, Lwt_list.filter_p, f);
let map_p_limited_concurrency = (~concurrency=20, f) =>
  list_p_limited_concurrency(~concurrency, Lwt_list.map_p, f);

// TODO: this shouldn't be here
module Uri = {
  include Uri;

  let to_yojson = t => `String(to_string(t));
  // TODO: exception here
  let of_yojson = json =>
    json |> [%of_yojson: string] |> Result.map(of_string);
};

// TODO: this shouldn't be here

module Set_with_yojson_make =
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

// TODO: this shouldn't be here

module Map_with_yojson_make =
       (
         K: {
           include Map.OrderedType;
           let to_yojson: t => Yojson.Safe.t;
           let of_yojson: Yojson.Safe.t => result(t, string);
         },
       ) => {
  include Map.Make(K);
  let to_yojson = (f, t) =>
    t |> to_seq |> List.of_seq |> [%to_yojson: list((K.t, 'a))](f);
  let of_yojson = (f, json) =>
    json
    |> [%of_yojson: list((K.t, 'a))](f)
    |> Result.map(l => l |> List.to_seq |> of_seq);
};

// TODO: this shouldn't be here
module Z = {
  include Z;
  let to_yojson = z => `String(Z.to_string(z));
  let of_yojson =
    fun
    | `String(string) =>
      try(Ok(Z.of_string(string))) {
      | _ => Error("failed to parse")
      }
    | _ => Error("invalid type");
};

// TODO: this shouldn't be here
module List = {
  include List;

  let rec find_index = (f, l) =>
    switch (l) {
    | [] => None
    | [hd, ..._] when f(hd) => Some(0)
    | [_, ...tl] => find_index(f, tl) |> Option.map((+)(1))
    };

  // TODO: please, stop this
  let dumb_uniq = (eq, l) =>
    l
    |> List.fold_left(
         (l', el) => List.exists(eq(el), l') ? l' : [el, ...l'],
         [],
       )
    |> List.rev;
};

module String_map =
  Map_with_yojson_make({
    [@deriving yojson]
    type t = string;
    let compare = String.compare;
  });

module String_set =
  Set_with_yojson_make({
    [@deriving yojson]
    type t = string;
    let compare = String.compare;
  });

module Int64_map =
  Map_with_yojson_make({
    [@deriving yojson]
    type t = int64;
    let compare = Int64.compare;
  });

module BLAKE2B = BLAKE2B;
module BLAKE2B_20 = BLAKE2B.BLAKE2B_20;
module Incremental_patricia = Incremental_patricia;
