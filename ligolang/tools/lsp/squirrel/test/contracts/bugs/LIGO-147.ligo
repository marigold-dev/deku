type a is record [x : int]
type b is record [y : a]

const x : Foo.Bar.ty = Foo.Bar.f (Foo.Bar.x, 0)
const y = block {
  const b = record [y = record [x = 42]]
} with b with record [x.y = 10]
