type foo_record = { foo : int ; bar : int }

let c = {
  foo = 1 ;
  bar = 2 ;
}

let a =
  let i = c.foo in
  i

let b =
  let j = c.boo in
  j
