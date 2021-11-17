let k = (x : int) => (_y : int) => x;

let k2 = (x : int) => (_ : int) => x;

let m = switch (Some(4)) {
  | Some(_x) => 1
  | None => 0
};
