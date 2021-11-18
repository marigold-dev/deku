let t12 = fun (x : int list) ->
  match x with
  | hd::(hd2::tl) -> hd + hd2
  | [] -> 0