let file_exists file =
  match Unix.stat file with _stat -> true | exception _ -> false

(* TODO: is run_in_systhread needed?*)
let spawn ~prog ~args k =
  Eio.Switch.run @@ fun sw ->
  let stdin_r, stdin_w = Unix.pipe ~cloexec:true () in
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let pid = Unix.create_process prog args stdin_r stdout_w Unix.stderr in

  (* TODO: a bit concerned about this as_socket*)
  let stdin =
    (Eio_unix.FD.as_socket ~close_unix:true ~sw stdin_w :> Eio.Flow.sink)
  in
  let stdout =
    (Eio_unix.FD.as_socket ~close_unix:true ~sw stdout_r :> Eio.Flow.source)
  in

  let () =
    Eio.Switch.on_release sw (fun () ->
        Unix.kill pid Sys.sigkill;
        Unix.close stdin_r;
        Unix.close stdin_w;
        Unix.close stdout_r;
        Unix.close stdout_w)
  in
  k ~stdin ~stdout
