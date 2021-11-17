module RopeImplementation = Rope_implementation

type impl = RopeImplementation.t

type 'a t =
  S     : string -> (((impl -> 'a) -> 'b) -> (impl -> 'a) -> 'b) t
| Other : 'a     -> 'a t

let _S =
  fun constant ->
  fun f ->
  fun new_cont ->
  f (fun (acc : impl) -> new_cont RopeImplementation.(cat acc (of_string constant)))

let z = Other _S
let _d =
  fun f ->
  fun new_cont ->
  f (fun (acc : impl) (i : int) -> new_cont RopeImplementation.(cat acc (of_string (string_of_int i))))
let d = Other _d

let _s =
  fun f ->
  fun new_cont ->
  f (fun (acc : impl) (s : string) -> new_cont RopeImplementation.(cat acc (of_string s)))
let s = Other _s

let start cont = cont RopeImplementation.(of_string "")
let finish (acc : impl) = acc

let (~%) : type a b . (((impl -> a) -> a) -> b) t -> b =
  function
  | S     str -> _S str start
  | Other  f  -> f start
let (%) : type a b . a -> (a -> b) t -> b = fun fmt1 fmt2 -> match fmt2 with
  | S     str -> _S str fmt1
  | Other f2  -> f2 fmt1
let (#%) fmt arg =
  (fmt finish) arg
