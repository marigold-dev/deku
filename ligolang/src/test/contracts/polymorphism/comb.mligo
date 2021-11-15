type 'a dup = 'a * 'a

let diag (type a) (x : a) : a dup = (x, x)

let rec rev (type a) ((xs, acc) : a list * a list) : a list =
  match xs with
  | [] -> acc
  | x :: xs -> rev (xs, (x :: acc))

let rev (type a) (xs : a list) : a list = rev (xs, ([] : a list))

let rec zip (type a b) ((xs, ys, acc) : a list * b list * (a * b) list) : (a * b) list =
  match xs, ys with
  | [], [] -> acc
  | x :: xs, y :: ys ->
    zip (xs, ys, ((x, y) :: acc))
  | _, _ -> failwith "oops"

let zip (type a b) (xs : a list) (ys : b list) : (a * b) list = rev (zip (xs, ys, ([] : (a * b) list)))

let self_zip (type tau) (xs : tau list) : (tau * tau) list =
  let (xs, ys) = diag xs in
  zip xs ys

let v : (string * string) list = self_zip ["a";"b"]
let w : (int * nat) list = zip [1;2;3] [4n;5n;6n]
