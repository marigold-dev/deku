(* This module defines and exports the type [t] of conditional
   expressions of preprocessing directives. *)

type t =
  Or    of t * t
| And   of t * t
| Eq    of t * t
| Neq   of t * t
| Not   of t
| True
| False
| Ident of string

(* Environments *)

module Env = Set.Make (String)

let rec eval env = function
   Or (e1,e2) -> eval env e1 || eval env e2
| And (e1,e2) -> eval env e1 && eval env e2
|  Eq (e1,e2) -> eval env e1 = eval env e2
| Neq (e1,e2) -> eval env e1 != eval env e2
|       Not e -> not (eval env e)
|        True -> true
|       False -> false
|    Ident id -> Env.mem id env
