open Trace

(* module Core = Typesystem.Core *)
open Ast_core.Types
(* open AinfAst_coreped.Reasons *)
open Ast_core.Combinators

let test_err s = Main_errors.test_internal s
let tst_assert s p = Assert.assert_true (test_err s) p

let alias a b = make_sc_alias a b
let constructor id original_id tv tag args = make_sc_constructor id original_id tv tag args
let row ?(row=[]) id tv = make_sc_row id None tv C_record row
let tc tc args = make_sc_typeclass tc args
let poly tv forall = make_sc_poly tv forall
let access_label tv ~record_type label = make_sc_access_label tv ~record_type label

module Test_vars = struct
  let tva : type_variable = Var.of_name "a"
  let tvb : type_variable = Var.of_name "b"
  let tvc : type_variable = Var.of_name "c"
  let tvd : type_variable = Var.of_name "d"
  let tve : type_variable = Var.of_name "e"
  let tvf : type_variable = Var.of_name "f"
  let labelfoo : label = Label "foo"
  let labelbar : label = Label "bar"
end
