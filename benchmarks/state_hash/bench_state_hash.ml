open Core_bench
open Core

let make_test f map info =
  Bench.Test.create ~name:info (fun () ->
      let _ = f map in
      () )

(*let test_params =
  [ (1_000, 5)
  ; (5_000, 5)
  ; (10_000, 5)
  ; (50_000, 5)
  ; (100_000, 5)
  ; (1_000, 25)
  ; (5_000, 25)
  ; (10_000, 25)
  ; (50_000, 25)
  ; (100_000, 25)
  ; (1_000, 50)
  ; (5_000, 50)
  ; (10_000, 50)
  ; (50_000, 50) ]*)

let test_params_big =
  [ (* 0.3 Gb: measure by major words of the first run results *)
    (*(100_000, 25)*)
    (* 1 millon time *)
    (100_000_000, 1) ]

let maps_yojson =
  List.map test_params_big ~f:(fun (l, r) ->
      (Map_hash.Bench_yojson.generate_maps l r, l, r) )

let maps_bin_prot =
  List.map test_params_big ~f:(fun (l, r) ->
      (Map_hash.Bench_bin_prot.generate_maps l r, l, r) )

let tests =
  List.fold2_exn maps_yojson maps_bin_prot ~init:[]
    ~f:(fun tests (jm, jl, jr) (bm, bl, br) ->
      let yojson =
        make_test Map_hash.Bench_yojson.hash_map jm
          (Printf.sprintf "yojson : %d, %d" jl jr)
      in
      let bin_prot =
        make_test Map_hash.Bench_bin_prot.hash_map bm
          (Printf.sprintf "bin_prot : %d, %d" bl br)
      in
      yojson :: bin_prot :: tests )
  |> List.rev

let command = Bench.make_command tests

let main () =
  Command_unix.run (Command.group ~summary:"" [("state-hash", command)])

let () = main ()
