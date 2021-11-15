(* This file implements the type-level language. For now limited to
   type constants, type functions and their application. *)

open Ast_core.Types

(** Evaluates a type-leval application. For now, only supports
    immediate beta-reduction at the root of the type. *)
let type_level_eval : type_value -> type_value * type_constraint list =
  fun tv -> Typesystem.Misc.Substitution.Pattern.eval_beta_root ~tv

(** Checks that a type-level application has been fully reduced. For
    now, only some simple cases like applications of `forall`
    <polymorphic types are allowed. *)
let check_applied ((reduced, _new_constraints) as x) =
  let () = match (reduced: type_value) with
      { location = _ ; wrap_content = P_apply _ } -> failwith "internal error: shouldn't happen" (* failwith "could not reduce type-level application. Arbitrary type-level applications are not supported for now." *)
    | _ -> ()
  in x
