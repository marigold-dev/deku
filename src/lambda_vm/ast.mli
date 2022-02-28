type ident = string

type prim =
  | Neg
  | Add
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
      (* condition <> 0 ? then_ : else_ *)
      condition : expr;
      then_ : expr;
      else_ : expr;
    }
  (* memory *)
  | Pair  of {
      left : expr;
      right : expr;
    }
  | Fst   of expr
  | Snd   of expr

type value =
  | Int64 of int64
  | Pair  of value * value

type script = {
  param : ident;
  code : expr;
}
