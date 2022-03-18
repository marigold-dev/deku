open Core
open Core_bench

let t1 = Bench.Test.create ~name:"Id" (fun () -> ())

let get_float () = if Random.bool () then 10.0 else 10.0

let get_int () = Random.int 200000

let scale t mul = Caml.int_of_float (Caml.float_of_int t *. mul)

let t2 =
  let n = get_int () in
  let fl = get_float () in
  Bench.Test.create ~name:"integer scaling" (fun () -> ignore (scale n fl))

let tests = [t1; t2]
let command = Bench.make_command tests

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks" [("basic", command)])

let () = main ()
