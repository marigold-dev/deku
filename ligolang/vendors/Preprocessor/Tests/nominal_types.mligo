type foo_variant = Foo of int | Bar of string
type foo_record = { foo : foo_variant ; bar : foo_variant}

let a = Foo 0

let b = Bar ""

let c = {
  foo = a ;
  bar = b ;
}

let main (p : foo_record) : foo_variant = p.foo

