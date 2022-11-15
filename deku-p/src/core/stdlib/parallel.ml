module Pool = struct
  type task = Task : ('a, exn) result Eio.Promise.u * (unit -> 'a) -> task
  type pool = { domains : int; pending : task Eio.Stream.t }

  let rec domain_loop ~sw pending =
    (* TODO: handle cancelation *)
    let (Task (resolver, compute)) = Eio.Stream.take pending in
    ( Eio.Fiber.fork ~sw @@ fun () ->
      let result = try Ok (compute ()) with exn -> Error exn in
      Eio.Promise.resolve resolver result );
    domain_loop ~sw pending

  let domain_loop pending = Eio.Switch.run @@ fun sw -> domain_loop ~sw pending

  let parallel pool compute =
    let promise, (resolver : ('a, exn) result Eio.Promise.u) =
      Eio.Promise.create ()
    in
    Eio.Stream.add pool.pending (Task (resolver, compute));
    match Eio.Promise.await promise with
    | Ok value -> value
    | Error exn -> raise exn

  let pool_ref : pool option ref = ref None

  let run ~env ~domains f =
    let domains = max domains 1 in
    Eio.Switch.run @@ fun sw ->
    let domain_mgr = Eio.Stdenv.domain_mgr env in
    let pending = Eio.Stream.create 1 in
    let spawn () =
      Eio.Domain_manager.run domain_mgr (fun () -> domain_loop pending)
    in
    let create_domains = List.init domains (fun _n -> spawn) in
    (* TODO: maybe this should be fork daemon? *)
    ( Eio.Fiber.fork_daemon ~sw @@ fun () ->
      Eio.Fiber.all create_domains;
      `Stop_daemon );
    let pool = { domains; pending } in
    pool_ref := Some pool;
    f ()

  let get () =
    match !pool_ref with
    | Some pool -> pool
    | None -> failwith "Not running Parallel.Pool.run"
end

module Worker = struct
  type worker = (unit -> unit) Eio.Stream.t
  type t = worker

  let rec loop ~sw stream =
    (* TODO: this may leak workers *)
    let task = Eio.Stream.take stream in
    Eio.Fiber.fork ~sw task;
    loop ~sw stream

  let make ~domains ~sw =
    let stream = Eio.Stream.create 0 in
    let spawn () =
      Eio.Domain_manager.run domains (fun () ->
          Eio.Switch.run @@ fun sw -> loop ~sw stream)
    in
    Eio.Fiber.fork ~sw spawn;
    stream

  let schedule worker task =
    let promise, resolver = Eio.Promise.create () in
    let task () =
      match task () with
      | value -> Eio.Promise.resolve_ok resolver value
      | exception exn -> Eio.Promise.resolve_error resolver exn
    in
    Eio.Stream.add worker task;
    Eio.Promise.await_exn promise
end

let map_p f l =
  let Pool.({ domains; pending = _ } as pool) = Pool.get () in
  let length = List.length l in
  let chunk_size = max (length / domains) 1 in
  let chunks = Base.List.chunks_of l ~length:chunk_size in
  let chunks =
    Eio.Fiber.List.map
      (fun chunk -> Pool.parallel pool (fun () -> List.map f chunk))
      chunks
  in
  List.concat chunks

let init_p n f =
  let l = List.init n (fun x -> x) in
  map_p f l

let filter_map_p f l =
  let l = map_p f l in
  List.filter_map (fun x -> x) l

let parallel task =
  let pool = Pool.get () in
  Pool.parallel pool task
