open Ast_core
open Errors
open Trace

let assert_type_expression_eq ~raise (loc:Location.t) ((tv',tv):type_expression * type_expression) : unit = 
  trace_option ~raise (assert_equal loc tv' tv) @@
    assert_type_expression_eq (tv' , tv)

type typer = type_expression list -> type_expression option -> type_expression

let typer_0 ~raise : Location.t -> string -> (raise:typer_error raise -> type_expression option -> type_expression) -> typer = fun l s f lst tv_opt ->
  match lst with
  | [] -> f ~raise tv_opt
  | _ -> raise.raise @@ wrong_param_number l s 0 lst

let typer_1 ~raise : Location.t -> string -> (raise:typer_error raise -> type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ] -> f ~raise a
  | _ -> raise.raise @@ wrong_param_number l s 1 lst

let typer_1_opt ~raise : Location.t -> string -> (raise:typer_error raise -> type_expression -> type_expression option -> type_expression) -> typer = fun l s f lst tv_opt ->
  match lst with
  | [ a ] -> f ~raise a tv_opt
  | _ -> raise.raise @@ wrong_param_number l s 1 lst

let typer_2 ~raise : Location.t -> string -> (raise:typer_error raise -> type_expression -> type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ] -> f ~raise a b
  | _ -> raise.raise @@ wrong_param_number l s 2 lst

let typer_2_opt ~raise : Location.t -> string -> (raise:typer_error raise -> type_expression -> type_expression -> type_expression option -> type_expression) -> typer = fun l s f lst tv_opt ->
  match lst with
  | [ a ; b ] -> f ~raise a b tv_opt
  | _ -> raise.raise @@ wrong_param_number l s 2 lst

let typer_3 ~raise : Location.t -> string -> (raise:typer_error raise -> type_expression -> type_expression -> type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ; c ] -> f ~raise a b c
  | _ -> raise.raise @@ wrong_param_number l s 3 lst

let typer_4 ~raise : Location.t -> string -> (raise:typer_error raise -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ; c ; d ] -> f ~raise a b c d
  | _ -> raise.raise @@ wrong_param_number l s 4 lst

let typer_5 ~raise : Location.t -> string -> (raise:typer_error raise -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ; c ; d ; e ] -> f ~raise a b c d e 
  | _ -> raise.raise @@ wrong_param_number l s 5 lst

let typer_6 ~raise : Location.t -> string
  -> (raise:typer_error raise -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ; c ; d ; e ; f_ ] -> f ~raise a b c d e f_
  | _ -> raise.raise @@ wrong_param_number l s 6 lst

let constant' ~raise loc name cst = typer_0 ~raise loc name (fun ~raise:_ -> cst)
let eq_1 a cst = type_expression_eq (a , cst)
let eq_2 (a , b) cst = type_expression_eq (a , cst) && type_expression_eq (b , cst)

let assert_eq_1 ~raise ?(l=(Location.Virtual "assert_eq_1")) a b = if eq_1 a b then () else raise.raise @@ not_matching l a b
