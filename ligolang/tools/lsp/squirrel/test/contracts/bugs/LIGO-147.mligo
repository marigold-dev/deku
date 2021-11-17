type a = {x: int}
type b = {y: a}

let x : Foo.Bar.ty = Foo.Bar.f (Foo.Bar.x, 0)
let y =
  let b = {y = {x = 42}} in
  {b with y.x = 10}
