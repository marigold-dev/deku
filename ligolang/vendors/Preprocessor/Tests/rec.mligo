let a = 1

let b =
  let rec c : int*int -> int = fun ((i,j):int*int) ->
    let k = i + j + a in
    c (k,1)
  in
  let b = 2 in
  c (a, b)
