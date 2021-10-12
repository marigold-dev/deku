include List;

let find_index = (f, l) => {
  let rec go = idx => {
    fun
    | [] => None
    | [x, ...xs] =>
      if (f(x)) {
        Some(idx);
      } else {
        go(idx + 1, xs);
      };
  };
  go(0, l);
};

let in_order_uniq = (type a, compare, l) => {
  module S =
    Set.Make({
      type t = a;
      let compare = compare;
    });
  let rec go = (acc, seen_set, lst) => {
    switch (lst) {
    | [] => List.rev(acc)
    | [x, ...xs] =>
      if (S.mem(x, seen_set)) {
        go(acc, seen_set, xs);
      } else {
        go([x, ...acc], S.add(x, seen_set), xs);
      }
    };
  };
  go([], S.empty, l);
};

let rec fold_left_ok = (f, state) =>
  fun
  | [] => Ok(state)
  | [head, ...tl] =>
    switch (f(state, head)) {
    | Ok(state) => fold_left_ok(f, state, tl)
    | Error(error) => Error(error)
    };
