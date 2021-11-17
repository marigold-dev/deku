let rec j (x : int) : int =
  if x < 0 then
    (failwith "negative" : int)
  else
    j (x - 1)

let g (x : int) = x

let test_me = (fun (f : int -> int) -> f (g 4)) j
