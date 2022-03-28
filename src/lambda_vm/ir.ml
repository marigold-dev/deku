type ident = Ident.t [@@deriving yojson, show, eq]

type prim =
  | P_neg
  | P_add
  | P_sub
  | P_mul
  | P_div
  | P_rem
  | P_land
  | P_lor
  | P_lxor
  | P_lsl
  | P_lsr
  | P_asr
  | P_fst
  | P_snd
[@@deriving show, yojson, eq]

module Env = struct
  include Map_with_cardinality.Make (Ident)

  let pp _ fmt _ = Format.fprintf fmt "<env>"
end

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
[@@deriving show, yojson, eq]

type value =
  | V_int64     of int64
  | V_pair      of {
      first : value;
      second : value;
    }
  | V_closure   of {
      env : value Env.t;
      param : Ident.t;
      body : expr;
    }
  | V_primitive of {
      args : value list;
      prim : prim;
    }
[@@deriving show, yojson, eq]

type script = {
  param : Ident.t;
  code : expr;
}
[@@deriving yojson, show, eq]
