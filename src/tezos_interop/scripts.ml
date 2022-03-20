open Helpers

let tezos_js_bridge = [%blob "tezos_js_bridge.bundle.js"]
let listen_transactions = [%blob "listen_transactions.bundle.js"]

let make_file ~prefix content =
  let%await file, oc = Lwt_io.open_temp_file ~prefix ~suffix:".js" () in
  let%await () = Lwt_io.write oc content in
  await file

let file_tezos_js_bridge, file_listen_transactions =
  Lwt_main.run
    (let%await file_tezos_js_bridge =
       make_file ~prefix:"tezos_js_bridge" tezos_js_bridge in
     let%await file_listen_transactions =
       make_file ~prefix:"listen_transactions" listen_transactions in
     Lwt.return (file_tezos_js_bridge, file_listen_transactions))
