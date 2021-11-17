type foobar = int * int
let test_t : foobar = 10, 25
let foo, bar = test_t

let type_tuple_d (_ : unit) = foo + bar

type complex = string * int * string * nat
let test_t_2 = "hello", 10, "world", 50n
let hello, ten, world, fifty_n = test_t_2

let type_tuple_d_2 (_ : unit) = hello ^ world
