(* Prepare the file generate for memtrace with the 'ctf' extension
      This file will be store in the folder /usr/tmp when execute:

      dune exec -- ./test.exe

      REMARK TODO:
      Running this raise an error in deku:
      esy x dune exec ~/deku/_build/default/benchmarks/memtrace/test.exe
      Entering directory '/home/quyen/deku'
     Fatal error: exception Failure("Gc.memprof.start: not implemented in multicore")

     even if it raises error it also generate the ctf file (but no data)

     using : esy x dune build

     For the test alone without interact with deku code, the test run:
     dune build
     dune exec ./test.exe

   TODO: remove by make clean all the file *.ctf (if we need to clean up)
*)

(*
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
  let filename = Filename.temp_file 
  ~temp_dir:"/home/quyen/deku/benchmarks/memtrace" "memtrace_test" ".ctf" in
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
*)

let test_fork ~quick_exit () =
  let filename =
    (* TODO give the home user path *)
    Filename.temp_file ~temp_dir:"/home/quyen/deku/benchmarks/memtrace"
      "memtrace_fork" ".ctf" in
  Unix.putenv "MEMTRACE" filename;
  (* Start memtrace tracing *)
  let trace = Memtrace.start_tracing ~context:None ~sampling_rate:1. ~filename in
  (* set up the allocation: before, after, child *)
  let alloc_before = 1234
  and alloc_after = 7364
  and alloc_child = 42 in
  (* starting from alloc before -> child -> after *)
  (*
     Sys.opaque_identity : 'a -> 'a
     For the purposes of optimatization, opaque_identity behaves like an unknown
     (thus possibly side-effecting) function
     A typical use of this function is to prevent pure computations from begin
     optimized away in benchmarking loops. for example:
     for _round = 1 to 100_000 do
         ignore (Sys.opaque_identity (my_pure_computation ()))
     done
     The example below the pure function we want to get is:
     Array.make alloc_before "a": create an array size:alloca_before fill with "a"
  *)
  let _ = Sys.opaque_identity Array.make alloc_before "a" in
  (* alloc child *)
  (* trace the unix fork command
      val fork: unit -> int
      For a new process. The returned interger is 0 for the child process,
      the pid of the child process for the parent process.
  *)
  begin
    match Unix.fork () with
    | 0 ->
      let count = if quick_exit then 1 else 1_000_000 in
      for _i = 1 to count do
        ignore (Sys.opaque_identity Array.make alloc_child "a")
      done;
      exit 0
    | pid ->
    (* Unix.waitpid: wait_flag list -> int -> int * process_status
       Same as wait (wait until one oe fhte children processes die, and returns its pid
       and termination status), but waits for the child process whose pid is given.
       if a pid is:
         -`-1`: means wait for the chlid.
         -`0`: means wait for any child in the same process group as the current process.
         - negative pid argument: represent process groups.
       The list of options indicates whethere `waitpid` should return immediately without
       waiting, and whether it should report stopped children.
    *)
    match Unix.waitpid [] pid with
    | _, WEXITED 0 -> ()
    | _ -> assert false
  end;
  (* alloc after *)
  let _ = Sys.opaque_identity Array.make alloc_after "a" in
  (* stop memtrace trace *)
  Memtrace.stop_tracing trace;

  (* Memtrace.Trace modules:
     - Writer: writing traces
     - Reader: reading traces
  *)

  (* create module R from Memetrace.Trace.Reader *)
  let module R = Memtrace.Trace.Reader in
  (* val open_: filename:string -> t *)
  let trace = R.open_ ~filename in
  (* create a hashtable of size 20 *)
  let sizes = Hashtbl.create 20 in
  (* iterate over a trace
     iter: t -> ?parse_backtraces:bool -> (Timedelta.t -> Event.t -> unit) -> unit
  *)
  R.iter trace (fun _time ev ->
      match ev with
      | Alloc a ->
        Hashtbl.add sizes a.length ()
        (* add the allocation length a into the hash table*)
      | _ -> ());

  (* check the size of alloc_before/alloca_after is it a member of the hash table *)
  assert (Hashtbl.mem sizes alloc_before);
  assert (Hashtbl.mem sizes alloc_after);
  assert (not (Hashtbl.mem sizes alloc_child));
  ()

let () = test_fork ~quick_exit:false ()
let () = test_fork ~quick_exit:true ()
