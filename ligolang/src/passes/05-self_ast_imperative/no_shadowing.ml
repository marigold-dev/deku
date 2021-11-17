open Ast_imperative
open Errors
open Trace

(* Prevents shadowing in the same scope. Needed for JsLIGO. *)

let rec check_block_scope ~raise vars types e = 
   match e.expression_content with
   | E_let_in {let_binder; rhs; let_result; _} -> 
      let var = Location.unwrap let_binder.var in
      if (List.mem ~equal:Var.equal vars var) then
         raise.raise @@ no_shadowing e.location
      else (
         check_block_scope ~raise [] [] rhs;
         check_block_scope ~raise (var :: vars) types let_result
      )
   | E_type_in {type_binder; let_result; _} ->
      if (List.mem ~equal:Var.equal types type_binder) then
         raise.raise @@ no_shadowing e.location
      else 
         check_block_scope ~raise vars (type_binder :: types) let_result
   | E_mod_in {module_binder; let_result; _} ->
      let var = Var.of_name module_binder in
      if (List.mem ~equal:Var.equal vars var) then
         raise.raise @@ no_shadowing e.location
      else (
         check_block_scope ~raise (var :: vars) types let_result
      )
   | _ -> ()
 
let peephole_expression ~raise : expression -> expression = fun e ->
   check_block_scope ~raise [] [] e;
   e

let peephole_module ~raise : module_ -> module_ = fun m ->
   let rec aux vars types = function  
      Location.{wrap_content = Declaration_type t; location} :: remaining -> 
         if (List.mem ~equal:Var.equal types t.type_binder) then 
            raise.raise @@ no_shadowing location
         else 
            aux vars (t.type_binder :: types) remaining
   |  {wrap_content = Declaration_constant t; location} :: remaining ->
         let var = Location.unwrap t.binder.var in
         if (List.mem ~equal:Var.equal vars var) then 
            raise.raise @@ no_shadowing location
         else 
            aux (var :: vars) types remaining
   |  {wrap_content = Declaration_module t; location} :: remaining ->
            let var = Var.of_name t.module_binder in
            if (List.mem ~equal:Var.equal vars var) then 
               raise.raise @@ no_shadowing location
            else 
               aux (var :: vars) types remaining
   |  {wrap_content = Module_alias _; _} :: remaining -> 
         aux vars types remaining
   | [] -> ()
   in
   aux [] [] m;
   m