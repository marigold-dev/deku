module H = struct
  let k (type a b) (x : a) (y : b) : a = x
end

let k (type a) (x : a) (y : a) : a = x

let test_helpers =
  let v = H.k 1 2 in
  assert (v = 1)
