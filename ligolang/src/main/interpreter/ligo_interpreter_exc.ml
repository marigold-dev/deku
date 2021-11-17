(* exceptions for meta and object languages *)

let throw_obj_exc loc calltrace =
  fun x ->
    let errs = List.map ~f:( fun e -> match e with `Tezos_alpha_error a -> a) x in
    Errors.target_lang_error loc calltrace errs
