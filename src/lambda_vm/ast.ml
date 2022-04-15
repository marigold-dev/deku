type ident = string

type prim =
  | Neg
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Land
  | Lor
  | Lxor
  | Lsl
  | Lsr
  | Asr
  | Fst
  | Snd

type expr =
  (* calculus *)
  | Var   of ident
  | Lam   of ident * expr
  | App   of {
      funct : expr;
      arg : expr;
    }
  (* prims *)
  | Const of int64
  | Prim  of prim
  (* branching *)
  | If    of {
      (* predicate <> 0 ? consequent : alternative *)
      predicate : expr;
      consequent : expr;
      alternative : expr;
    }
  (* memory *)
  | Pair  of {
      first : expr;
      second : expr;
    }

type value =
  | Int64 of int64
  | Pair  of value * value

type script = {
  param : ident;
  code : expr;
}
