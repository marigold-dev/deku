type foobar is
  Foo of int
| Bar of bool
| Kee of nat

const foo : foobar = Foo (42)

const bar : foobar = Bar (True)

const kee : foobar = Kee (23n)
