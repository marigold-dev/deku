(*
   Build a `Contract.t`

   type t =
   {
     code: Lambda_vm.Ir.code;
     storage: Lamdba_vm.Ir.value
   }

  type code =
  {
    param: Ident.t;
    code: expr
  }

  type value =
   | V_int64 of int64
   | V_pair of {
     first: value;
     second: value
   }
   | V_closure of {
     env: value Env.t;
     param: Ident.t;
     body: expr
    }
   | V_primitive of {
     args: value list;
     prim: prim
   }
*)

(* a. Build code type
   type expr =
   | E_var of ident
   | E_lam of ident * expr
   | E_app of {
     funt: expr;
     arg: expr
   }
   | E_const of int64
   | E_prim of prim
   | E_if of {
     predicate: expr;
     consequent: expr;
     alternative: expr
   }
   | E_pair  of {
     first: expr;
     second: expr
     }
   }
*)

let build_code_pair () : Lambda_vm.Ir.code =
  let param = 1 in
  let code = [%lamda_vm.script fun pair -> (fst pair + snd pair, (0L, 0L))] in
  { param; code }

(* b. build storage: lambda_vm.ir.value
   type value =
   | V_int64 of int64
   | V_pair of {
     first: value;
     second: value
   }
   | V_closure of {
     env: value Env.t;
     para: Ident.t;
     body: expr
   }
   | V_primitive of {
     args: value list;
     prim: prim
   }
*)

let build_storage_value () : Lambda_vm.Ir.value =
  let first = V_int64 1L in
  let second = V_int64 2L in
  V_pair { first; second }

(* c. Build type contract *)

let build_contract () : Contract.t =
  let code = build_code_pair () in
  let storage = build_storage_value () in
  { code; storage }
