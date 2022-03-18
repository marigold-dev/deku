open Helpers

let fetch_storage = [%blob "fetch_storage.bundle.js"]
let run_entrypoint = [%blob "run_entrypoint.bundle.js"]
let listen_transactions = [%blob "listen_transactions.bundle.js"]

let make_file ~prefix content =
  let%await file, oc = Lwt_io.open_temp_file ~prefix ~suffix:".js" () in
  let%await () = Lwt_io.write oc content in
  await file

let file_fetch_storage, file_run_entrypoint, file_listen_transactions =
  Lwt_main.run
    (let%await file_fetch_storage =
       make_file ~prefix:"fetch_storage" fetch_storage in
     let%await file_run_entrypoint =
       make_file ~prefix:"run_entrypoint" run_entrypoint in
     let%await file_listen_transactions =
       make_file ~prefix:"listen_transactions" listen_transactions in
     Lwt.return
       (file_fetch_storage, file_run_entrypoint, file_listen_transactions))
