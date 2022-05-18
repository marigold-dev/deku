open Helpers

let spawn ~file ~on_error ~on_close input_stream =
  let node = "node" in
  let command = (node, [|node; file|]) in

  let process = Lwt_process.open_process command in

  let on_error exn =
    on_error exn ;
    raise exn
  in

  (* handle process failure *)
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          (* this should never return *)
          let%await status = process#status in
          Lwt.return (on_close status))
        on_error) ;

  (* TODO: test that exception on input and output kill the node *)
  let input_stream = Lwt_stream.map Yojson.Safe.to_string input_stream in
  (* handle input exceptions *)
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> Lwt_io.write_lines process#stdin input_stream)
        on_error) ;

  let output_stream = Lwt_io.read_lines process#stdout in
  let output_stream =
    Lwt_stream.map
      (fun line ->
        (* TODO: are exceptions on Lwt_stream.map safe? *)
        Yojson.Safe.from_string line)
      output_stream
  in
  (* handle output exceptions *)
  let output_stream =
    Lwt_stream.map
      (function Ok value -> value | Error exn -> on_error exn)
      (Lwt_stream.wrap_exn output_stream)
  in
  output_stream
