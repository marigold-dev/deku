open Bin_prot.Std

type ident = Ident.t [@@deriving yojson, show, eq, bin_io]

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
  | P_sender
[@@deriving show, yojson, eq, bin_io]

let bin_shape_prim = failwith "Not used by Pollinate"

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
[@@deriving show, yojson, eq, bin_io]

let bin_shape_expr = failwith "Not used by Pollinate"

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
[@@deriving show, yojson, eq, bin_io]

let bin_shape_value = failwith "Not used by Pollinate"

let __bin_write_value__ = failwith "Not used by Pollinate"

type code = {
  param : Ident.t;
  code : expr;
}
[@@deriving yojson, show, eq, bin_io]

let bin_shape_code = failwith "Not used by Pollinate"

let __bin_write_code__ = failwith "Not used by Pollinate"
