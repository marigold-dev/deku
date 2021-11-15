type interpreter_error = Main_errors.all

let target_lang_failwith : Location.t -> Runned_result.failwith -> interpreter_error =
  fun loc e -> `Main_interpret_target_lang_failwith (loc,e)

let target_lang_error : Location.t -> Ligo_interpreter.Types.calltrace -> Tezos_error_monad__TzCore.error list -> interpreter_error =
  fun loc calltrace e -> `Main_interpret_target_lang_error (loc,calltrace,e)

let meta_lang_eval : Location.t -> Ligo_interpreter.Types.calltrace -> string -> interpreter_error =
  fun loc calltrace s -> `Main_interpret_meta_lang_eval (loc,calltrace,s)

let meta_lang_failwith : Location.t -> Ligo_interpreter.Types.calltrace -> Ligo_interpreter.Types.value -> interpreter_error =
  fun loc calltrace v -> `Main_interpret_meta_lang_failwith (loc,calltrace,v)

let test_entry_not_found : string -> interpreter_error = fun s ->
  `Main_interpret_test_entry_not_found s

let bootstrap_not_enough : Location.t -> interpreter_error = fun l ->
  `Main_interpret_boostrap_not_enough l

let generic_error : Location.t -> string -> interpreter_error = fun loc desc ->
  `Main_interpret_generic (loc,desc)

let not_enough_initial_accounts : Location.t -> Memory_proto_alpha.Protocol.Alpha_context.Tez.tez -> interpreter_error = fun loc max ->
  `Main_interpret_not_enough_initial_accounts (loc,max)

let literal : Location.t -> Ast_typed.literal -> interpreter_error = fun s l ->
  `Main_interpret_literal (s, l)

let modules_not_supported : Location.t -> interpreter_error = fun l ->
  `Main_interpret_modules_not_supported l
let corner_case ?(loc = Location.generated) () = generic_error loc "Corner case, please report to devs."
