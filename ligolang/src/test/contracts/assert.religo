let main = (p: bool,s: unit) => {
  let _ : unit = assert (p);
  ([]: list (operation), s);
};

let some = (o : option(unit)) => {
  assert_some(o)
};
