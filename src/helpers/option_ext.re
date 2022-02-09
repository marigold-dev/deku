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
