open Core_bench

let bench_big_map_hash =
  Bench.Test.create ~name:"consensus: hash big map" (fun () ->
      let _ = Map_hash.hash_the_map 200_000 5 in
      ())

let tests = [bench_big_map_hash]

let command = Bench.make_command tests
