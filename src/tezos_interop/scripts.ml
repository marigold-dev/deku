open Helpers

let tezos_js_bridge = [%blob "tezos_js_bridge.bundle.js"]

let file_tezos_js_bridge =
  Lwt_main.run
    (let%await file, oc =
       Lwt_io.open_temp_file ~prefix:"tezos_js_bridge" ~suffix:".js" ()
     in
     let%await () = Lwt_io.write oc tezos_js_bridge in
     await file)
