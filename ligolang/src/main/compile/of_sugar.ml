open Ast_sugar
open Desugaring

let compile (m : module_) : Ast_core.module_  =
  compile_module m

let compile_expression (e : expression) : Ast_core.expression  =
  compile_expression e

let list_declarations (m : module_) : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match el.wrap_content with
      | Declaration_constant {binder;_} -> (Var.to_name binder.var.wrap_content)::prev
      | _ -> prev)
    ~init:[] m
