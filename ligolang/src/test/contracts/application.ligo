// Test different ways of calling functions in PascaLIGO

type foo is record [bar : int -> int]

function f (const i : int) : int is i

function g (const _ : unit) : int -> int is f

const r : foo = record [bar = f]

const x : int = f (42)
const y : int = r.bar (42)
const z : int = (g (unit))(42)
