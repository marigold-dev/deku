type recordi = { a : int list option ; b : int list }

let t13 = fun (x:recordi) ->
  match x with
  | { a = Some ([]) ; b = (hd::tl) } -> hd
  | { a = Some (hd::tl) ; b = [] } -> hd