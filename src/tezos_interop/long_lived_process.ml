open Deku_stdlib

(* TODO: magic number: 256mb *)
let max_size = 256 * 1024 * 1024

let spawn ~file k =
  let prog = "node" in
  let args = [| prog; file |] in
  IO.spawn ~prog ~args @@ fun ~stdin ~stdout ->
  Eio.Buf_write.with_flow stdin @@ fun write_buf ->
  let read_buf = Eio.Buf_read.of_flow ~max_size stdout in
  let read () =
    let line = Eio.Buf_read.line read_buf in
    Yojson.Safe.from_string line
  in
  let write json =
    let json = Yojson.Safe.to_string json in
    Eio.Buf_write.string write_buf json;
    Eio.Buf_write.char write_buf '\n'
  in
  k ~read ~write

let spawn ~file ~on_error ~on_json k =
  let dummy_write _json = () in
  let write_ref = ref dummy_write in
  let write json =
    try !write_ref json
    with exn -> Format.eprintf "spawn.write: %s\n%!" (Printexc.to_string exn)
  in
  Eio.Switch.run @@ fun sw ->
  let handler ~read ~write =
    let write json = Eio.Fiber.fork ~sw @@ fun () -> write json in
    let on_json json = Eio.Fiber.fork ~sw @@ fun () -> on_json json in

    write_ref := write;
    let rec loop () =
      let json = read () in
      on_json json;
      loop ()
    in
    loop ()
  in
  let rec loop () =
    try spawn ~file handler
    with exn ->
      write_ref := dummy_write;
      on_error exn;
      loop ()
  in

  let () = Eio.Fiber.fork ~sw @@ fun () -> k ~write in
  loop ()
