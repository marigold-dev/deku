type foo =
| Bar (int)
| Baz;

let main = (f : foo) : int =>
  switch (f) {
  | Bar (i) => i
  | Baz     => (-1)
  };
