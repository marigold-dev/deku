open Eio

type indexer =
  | Indexer of { pool : (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t }

type t = indexer

let etl_modules = ref []
let register_etl etl = etl_modules := etl :: !etl_modules

let make_database ~uri =
  List.iter
    (fun (module Etl : Etl_intf.S) ->
      let init = Caqti_eio.with_connection uri (Etl.init ()) |> Promise.await in
      match init with
      | Ok _ -> ()
      | Error err -> failwith (Caqti_error.show err))
    !etl_modules

let make ~uri =
  let () = make_database ~uri in
  match Caqti_eio.connect_pool uri with
  | Ok pool -> Indexer { pool }
  | Error err -> failwith (Caqti_error.show err)

let on_block ~state ~operations ~receipts (Indexer { pool }) =
  (* TODO: how to handle concurrency of multiple etl modules? *)
  List.iter
    (fun (module Etl : Etl_intf.S) ->
      Etl.on_block ~state ~operations ~receipts pool)
    !etl_modules
