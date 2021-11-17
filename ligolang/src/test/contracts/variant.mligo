type foobar =
| Foo of int
| Bar of bool
| Kee of nat

let foo : foobar = Foo 42
let bar : foobar = Bar true
let kee : foobar = Kee 23n
