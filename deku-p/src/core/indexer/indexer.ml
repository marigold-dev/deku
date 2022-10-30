open Eio

type indexer =
  | Indexer of { pool : (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t }

type t = indexer

let make_database ~uri =
  let init =
    Caqti_eio.with_connection uri (Account_balances.init ()) |> Promise.await
  in
  match init with Ok _ -> () | Error err -> failwith (Caqti_error.show err)

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
    [ (module Account_balances) ]
