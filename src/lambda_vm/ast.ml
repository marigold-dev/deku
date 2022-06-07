open Bin_prot.Std

type ident = string [@@deriving yojson, show, bin_io]

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
  | Sender
[@@deriving yojson, show, bin_io]

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
[@@deriving yojson, show, bin_io]

type value =
  | Int64 of int64
  | Pair  of value * value
[@@deriving yojson, show, bin_io]

type script = {
  param : ident;
  code : expr;
}
[@@deriving yojson, show, bin_io]

let value_of_string gas str =
  let append list item = Pair (item, list) in
  (* TODO: burn gas properly for conversions *)
  Gas.burn_constant gas;
  let rec aux idx list =
    if idx >= 0 then
      let c = str.[idx] |> Char.code |> Int64.of_int in
      Int64 c |> append list |> aux (idx - 1)
    else
      list in
  let length = String.length str in
  let length' = Int64 (Int64.of_int length) in
  let list = aux (length - 1) (Pair (Int64 0L, Int64 0L)) in
  Pair (length', list)
