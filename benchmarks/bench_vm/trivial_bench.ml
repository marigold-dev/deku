open Core_bench

let bench = 
  Bench.Test.create ~name:"trivial" (fun () -> ())


let command = Bench.make_command [bench]
