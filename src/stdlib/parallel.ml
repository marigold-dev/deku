open Domainslib

module Pool = struct
  type pool = { domains : int; pool : Task.pool }
  type t = pool

  let make ~domains =
    let pool = Task.setup_pool ~num_domains:domains () in
    { domains; pool }
end

let map_p pool f l =
  let Pool.{ domains; pool } = pool in

  let length = List.length l in
  let chunk_size = max (length / domains) 1 in
  let chunks = Base.List.chunks_of l ~length:chunk_size in
  Task.run pool (fun () ->
      let promises =
        List.map
          (fun chunk -> Task.async pool (fun () -> List.map f chunk))
          chunks
      in
      List.concat_map (fun promise -> Task.await pool promise) promises)

let init_p pool n f =
  let l = List.init n (fun x -> x) in
  map_p pool f l

let filter_map_p pool f l =
  let l = map_p pool f l in
  List.filter_map (fun x -> x) l

let async pool task =
  let Pool.{ domains = _; pool } = pool in
  let task_lazy = lazy (try Ok (task ()) with exn -> Error exn) in

  let waiter, wakener = Lwt.wait () in
  let id =
    Lwt_unix.make_notification ~once:true (fun () ->
        let task_result = Lazy.force task_lazy in
        Lwt.wakeup_result wakener task_result)
  in
  let _promise =
    Task.async pool (fun _ ->
        let _task_result = Lazy.force task_lazy in
        Lwt_unix.send_notification id)
  in
  waiter
