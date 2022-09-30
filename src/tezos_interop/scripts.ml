let tezos_js_bridge = [%blob "tezos_js_bridge.bundle.js"]

let file_tezos_js_bridge =
  let file = "tezos_js_bridge.bundle.js" in
  let () =
    Eio_main.run @@ fun env ->
    let cwd = Eio.Stdenv.cwd env in
    let file = Eio.Path.(cwd / file) in
    Eio.Path.save ~create:(`Or_truncate 0o644) file tezos_js_bridge
  in
  let cwd = Unix.getcwd () in
  Filename.concat cwd file
