[@inline] let x = 1
[@inline] let foo (a : int): int =
  ([@inline] let test = 2 + a in test)
[@inline][@other] let y = 1
let bar (b : int): int =
  [@inline][@foo][@bar] let test = fun (z : int) -> 2 + b + z in
  test b
