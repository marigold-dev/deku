include Option;
module Let_syntax = {
  let some = Option.some;
  let (let.none) = (v, f) =>
    switch (v) {
    | None => f()
    | Some(v) => Some(v)
    };
  let (let.some) = Option.bind;
  let (let.default) = (v, f) => Option.value(f(), ~default=v);
};
module Syntax = {
  let [@inline] prod =
    (m1, m2) =>
      switch (m1, m2) {
      | (Some(x), Some(y)) => Some((x, y))
      | _ => None
      };
  let ( let* ) = Option.bind;
  let ( and* ) = prod;
  let (let+) = Option.map;
  let (and+) = prod;
};
