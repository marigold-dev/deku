
type 'a result = Ok of 'a | Error of string
type t = result

let ret (type a) (x : a) : a result =
  Ok x

let fail (type a) (x : string) : a result =
  Error x

type t = result

let bind (type a b) (x : a result) (f : a -> b result) : b result =
  match x with
  | Error s -> Error s
  | Ok a -> f a

let run (type a) (m : a result) : a =
  match m with
  | Ok a -> a
  | Error s -> failwith s
