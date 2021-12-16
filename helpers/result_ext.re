include Result;

module Let_syntax = {
  let ok = Result.ok;
  let (let.ok) = Result.bind;
  let (let.assert) = ((message, bool), f) => bool ? f() : Error(message);
};
module Syntax = {
  let [@inline] prod =
    (m1, m2) =>
      switch (m1, m2) {
      | (Ok(x), Ok(y)) => Ok((x, y))
      | (Error(_) as err, _) => err
      | (_, Error(_) as err) => err
      };
  let ( let* ) = Result.bind;
  let ( and* ) = prod;
  let (let+) = Result.map;
  let (and+) = prod;
};
