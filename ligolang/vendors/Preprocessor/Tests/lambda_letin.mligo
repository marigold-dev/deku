let a = 1

let b =
  let f : (int -> int -> int) = fun (i : int) (j : int) ->
    let g = j + i + a in
    let k = j + i + a + g in
    j + i + a + g + k
  in
  f 2 a