open Simple_utils
open Trace
open Helpers
open Errors

(**
  check_view_type checks against michelson restriction (usually defined in tezos/src/proto_alpha/lib_protocol/script_ir_translator.ml)
**)
let check_view_type ~raise : err_data:(Location.t*string*string) -> contract_type -> view_type -> unit =
  fun ~err_data:(loc,main_name,view_name) {storage = c_storage ; _} {arg ; storage = v_storage ; return} ->
    let () = trace_option ~raise (storage_view_contract loc main_name view_name c_storage v_storage) @@
      Ast_typed.assert_type_expression_eq (c_storage,v_storage) in
    let open Stage_common.Constant in
    let type_check err (t: Ast_typed.type_expression) : unit =
      let forbidden = [big_map_name ; sapling_state_name ; operation_name ; ticket_name] in
      let aux (t: Ast_typed.type_expression) =
        match t.type_content with 
        | T_constant { injection ; _} ->
          List.iter
            ~f:(fun forbidden ->
              if String.equal (Ligo_string.extract injection) forbidden then raise.raise err
              else ()
            )
            forbidden
        | _ -> ()
      in
      Helpers.iter_type_expression aux t
    in
    let () = type_check (type_view_io_out loc return) return in
    let () = type_check (type_view_io_in loc arg) arg in
    ()