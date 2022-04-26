(* Prepare the file generate for memtrace with the 'ctf' extension
   This file will be store in the folder /usr/tmp when execute:

   dune exec -- ./test.exe

   REMARK TODO:
   Running this raise an error in deku:
   esy x dune exec ~/deku/_build/default/benchmarks/memtrace/test.exe
   Entering directory '/home/quyen/deku'     
  Fatal error: exception Failure("Gc.memprof.start: not implemented in multicore")


*)

let globs = Array.make 1000 [||]

let nglobs = ref 0

(* store ref of an array size 1000 starting with 0 then keep on increase it *)
let leak x =
  globs.(!nglobs) <- x;
  incr nglobs

let rec long_bt = function
  | 0 ->
    leak (Array.make 1000 0);
    (Sys.opaque_identity List.iter) (fun () -> leak (Array.make 1000 0)) [()];
    42
  | n ->
    if Random.bool () then
      1 + long_bt (n - 1)
    else
      2 + long_bt (n - 1)

let go () =
  let filename = Filename.temp_file "memtrace_test" ".ctf" in
  (* start tracing *)
  let trace =
    Memtrace.start_tracing ~context:(Some "ctxt") ~sampling_rate:0.1 ~filename
  in
  (* do something here for what I want to trace *)
  leak (Array.make 4242 42);
  for _i = 0 to 10 do
    (* long_bt is the function that I am tracing *)
    let n = long_bt 10_000 in
    assert (n > 0)
  done;

  (* stop tracing *)
  Memtrace.stop_tracing trace;

  (* after stop tracing, start to read the file *)
  let module R = Memtrace.Trace.Reader in
  let file_trace = R.open_ ~filename in
  (* Iterate inside the file trace *)
  let first = ref true in
  R.iter file_trace (fun _time ev ->
      match ev with
      | Alloc info when !first ->
        first := false;
        assert (info.length = 4242);
        ()
      | _ -> ());
  (* closing the reader trace *)
  R.close file_trace;
  Unix.unlink filename;
  assert (not !first)

let main = go ()
