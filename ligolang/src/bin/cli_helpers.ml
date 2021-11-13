
let return_good v =  Proto_alpha_utils.Error_monad.return v

exception Error_compilation
let return_bad v : unit Proto_alpha_utils.Error_monad.tzresult Lwt.t = (
  if v.[String.length v - 1] = '\n' then
    Format.eprintf "%s" v
  else
    Format.eprintf "%s\n" v;
    Format.pp_print_flush Format.err_formatter ();
    Proto_alpha_utils.Error_monad.fail_with_exn Error_compilation
  )


let return_result : ?warn:bool -> ?output_file:string -> ('value, _) result -> unit Proto_alpha_utils.Error_monad.tzresult Lwt.t =
  fun ?(warn=false) ?output_file value ->
    let return_with_warn warns f =
          if not (String.length (String.trim warns) = 0) && warn then
            begin
              Format.eprintf "%s\n" warns;
              Format.pp_print_flush Format.err_formatter ()
            end;
          f ()
    in
    match value with
    | Ok (v,w) ->
      let fmt : Format.formatter = match output_file with
        | Some file_path -> Format.formatter_of_out_channel @@ open_out file_path
        | None -> Format.std_formatter in
      return_with_warn w (fun () -> return_good @@ (Format.fprintf fmt "%s\n" v;
                                                  Format.pp_print_flush fmt ()))
    | Error (e,w) ->
       return_with_warn w (fun () -> return_bad e)
