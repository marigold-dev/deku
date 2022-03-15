type ident = string
[@@deriving show]

type prim =
  | Neg
  | Add
  | Add_with_carry
  | Sub
  | Mul
  | Div
  | Rem
  | And
  | Or
  | Xor
  | Lsl
  | Lsr
  | Asr
  [@@deriving show]

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
  | Fst   of expr
  | Snd   of expr
  [@@deriving show]

type value =
  | Int64 of int64
  | Pair  of value * value
  [@@deriving show]

type script = {
  param : ident;
  code : expr;
}
[@@deriving show]