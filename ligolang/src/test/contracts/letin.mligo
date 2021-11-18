type storage = int * int

let main (n : int * storage) : operation list * storage =
  let x : int * int =
    let x : int = 7
    in x + n.0, n.1.0 + n.1.1
  in ([] : operation list), x


let f0 (_a: string) = true
let f1 (_a: string) = true
let f2 (_a: string) = true

let letin_nesting (_: unit) =
  begin
    let s = "test" in
    let p0 = f0 s in
    assert p0;
    let p1 = f1 s in
    assert p1;
    let p2 = f2 s in
    assert p2;
    s
  end

let letin_nesting2 (x: int) =
  let y = 2 in
  let z = 3 in
  x + y + z

let x =
  let (_, (x, _)) = (1n, (2n, 3n)) in
  x
