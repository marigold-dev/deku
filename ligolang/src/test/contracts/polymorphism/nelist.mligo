type 'a nelist = 'a * 'a list

let to_list (type a) ((x, xs) : a nelist) : a list = x :: xs
let of_list (type a) (xs : a list) : a nelist = match xs with
  | [] -> failwith "nelist: empty list"
  | (x :: xs) -> (x, xs)

let singleton (type a) (hd : a) : a nelist = (hd, ([] : a list))
let hd (type a) (xs : a nelist) : a = xs.0
let cons (type a) (hd : a) (xs : a nelist) : a nelist = (hd, (xs.0 :: xs.1))
let iter (type a) (f : a -> unit) (xs : a nelist) : unit = let () = f xs.0 in List.iter f xs.1
let map (type a b) (f : a -> b) (xs : a nelist) : (b nelist) = (f xs.0, List.map f xs.1)
let fold_left (type a b) (f : (b * a) -> b) (init : b) (xs : a nelist) : b = List.fold_left f (f (init, xs.0)) xs.1
