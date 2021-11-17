type storage = unit;

let main = (_: unit, _ : storage) =>
  if (true) { failwith("This contract always fails"); };
