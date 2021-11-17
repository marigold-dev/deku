type foobar =
  Foo (int)
| Bar (bool)
| Kee (nat);

let foo: foobar = Foo (42);
let bar: foobar = Bar (true);
let kee: foobar = Kee (23n);
