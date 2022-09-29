open Domainslib

module Pool = struct
  type pool = { domains : int; pool : Task.pool }
  type t = pool

  let make ~domains =
    let pool = Task.setup_pool ~num_domains:domains () in
    { domains; pool }

  let run pool f =
    let { domains = _; pool } = pool in
    Task.run pool f
end

let map_p pool f l =
  let Pool.{ domains; pool } = pool in

  let length = List.length l in
  let chunk_size = max (length / domains) 1 in
  let chunks = Base.List.chunks_of l ~length:chunk_size in
  let promises =
    List.map (fun chunk -> Task.async pool (fun () -> List.map f chunk)) chunks
  in
  List.concat_map (fun promise -> Task.await pool promise) promises

let init_p pool n f =
  let l = List.init n (fun x -> x) in
  map_p pool f l

let filter_map_p pool f l =
  let l = map_p pool f l in
  List.filter_map (fun x -> x) l

let async pool task =
  let Pool.{ domains = _; pool } = pool in
  let promise, resolver = Eio.Promise.create () in
  let _promise =
    Task.async pool (fun () ->
        let task_result = task () in
        Eio.Promise.resolve resolver task_result)
  in
  promise
