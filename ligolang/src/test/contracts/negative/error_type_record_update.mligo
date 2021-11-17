type foo = {
  i : int;
  j : bool;
}

let bar (x : foo) : foo =
  let x = { x with i = x.j } in
  x
