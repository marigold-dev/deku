let compile_contract ~env ~lang ~filename_ligo ~filename_tz () =
  Eio.Switch.run @@ fun sw ->
  Logs.info (fun m -> m "compiling %s with syntax %s" filename_ligo lang);
  let stdout =
    Unix.open_process_args_in "ligo"
      [| "ligo"; "compile"; "contract"; filename_ligo |]
  in
  let descr = Unix.descr_of_in_channel stdout in
  let source =
    (Eio_unix.FD.as_socket ~sw ~close_unix:true descr :> Eio.Flow.source)
  in
  Eio_unix.await_readable descr;
  let sink =
    Eio.Path.open_out ~sw ~append:false ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.cwd env / filename_tz)
  in
  Eio.Flow.copy source sink

let compile_storage ~lang ~filename_ligo ~expression () =
  Logs.info (fun m ->
      m "compiling storage '%s' aginst file %s with syntax %s" expression
        filename_ligo lang);
  let stdout =
    Unix.open_process_args_in "ligo"
      [| "ligo"; "compile"; "storage"; filename_ligo; expression |]
  in
  In_channel.input_all stdout

let compile_parameter ~lang ~filename_ligo ~expression () =
  Logs.info (fun m -> m "compiling %s with syntax %s" filename_ligo lang);
  let stdout =
    Unix.open_process_args_in "ligo"
      [| "ligo"; "compile"; "parameter"; filename_ligo; expression |]
  in
  In_channel.input_all stdout
