type ident = Ident.t [@@deriving eq, show]

type prim =
  | P_neg
  | P_add
  | P_add_with_carry
  | P_sub
  | P_mul
  | P_div
  | P_rem
  | P_and
  | P_or
  | P_xor
  | P_lsl
  | P_lsr
  | P_asr
[@@deriving eq, show]

type expr =
  (* calculus *)
  | E_var   of ident
  | E_lam   of ident * expr
  | E_app   of {
      funct : expr;
      arg : expr;
    }
  (* prims *)
  | E_const of int64
  | E_prim  of prim
  (* branching *)
  | E_if    of {
      (* predicate <> 0 ? consequent : alternative *)
      predicate : expr;
      consequent : expr;
      alternative : expr;
    }
  (* memory *)
  | E_pair  of {
      first : expr;
      second : expr;
    }
  | E_fst   of expr
  | E_snd   of expr
[@@deriving eq, show]

module Ident_map = Map_with_cardinality.Make (Ident)

type value =
  | V_int64     of int64
  | V_pair      of {
      first : value;
      second : value;
    }
  | V_closure   of {
      env : env;
      param : Ident.t;
      body : expr;
    }
  | V_primitive of {
      args : value list;
      prim : prim;
    }
[@@deriving eq, show]

and env = (value Ident_map.t[@opaque])

type script = {
  param : Ident.t;
  code : expr;
}
[@@deriving show]
