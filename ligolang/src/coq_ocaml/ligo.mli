open Datatypes
open Co_de_bruijn

type ('a, 'op, 'lit) expr =
| E_var of 'a
| E_let_in of 'a * splitting * ('a, 'op, 'lit) expr * ('a, 'op, 'lit) binds
| E_tuple of 'a * ('a, 'op, 'lit) args
| E_let_tuple of 'a * splitting * ('a, 'op, 'lit) expr * ('a, 'op, 'lit) binds
| E_proj of 'a * ('a, 'op, 'lit) expr * nat * nat
| E_update of 'a * ('a, 'op, 'lit) args * nat * nat
| E_app of 'a * ('a, 'op, 'lit) args
| E_lam of 'a * ('a, 'op, 'lit) binds * ('a, string) Tezos_micheline.Micheline.node
| E_operator of 'a * 'op * ('a, 'op, 'lit) static_args * ('a, 'op, 'lit) args
| E_literal of 'a * 'lit
| E_pair of 'a * ('a, 'op, 'lit) args
| E_car of 'a * ('a, 'op, 'lit) expr
| E_cdr of 'a * ('a, 'op, 'lit) expr
| E_unit of 'a
| E_left of 'a * ('a, string) Tezos_micheline.Micheline.node * ('a, 'op, 'lit) expr
| E_right of 'a * ('a, string) Tezos_micheline.Micheline.node * ('a, 'op, 'lit) expr
| E_if_left of 'a * ('a, 'op, 'lit) cond
| E_if_bool of 'a * ('a, 'op, 'lit) cond
| E_if_none of 'a * ('a, 'op, 'lit) cond
| E_if_cons of 'a * ('a, 'op, 'lit) cond
| E_iter of 'a * splitting * ('a, 'op, 'lit) binds * ('a, 'op, 'lit) expr
| E_map of 'a * splitting * ('a, 'op, 'lit) binds * ('a, 'op, 'lit) expr
| E_loop_left of 'a * splitting * ('a, 'op, 'lit) binds
   * ('a, string) Tezos_micheline.Micheline.node * ('a, 'op, 'lit) expr
| E_fold of 'a * splitting * ('a, 'op, 'lit) expr * splitting * ('a, 'op, 'lit) expr
   * ('a, 'op, 'lit) binds
| E_fold_right of 'a * ('a, string) Tezos_micheline.Micheline.node * splitting
   * ('a, 'op, 'lit) expr * splitting * ('a, 'op, 'lit) expr * ('a, 'op, 'lit) binds
| E_failwith of 'a * ('a, 'op, 'lit) expr
| E_raw_michelson of 'a * ('a, string) Tezos_micheline.Micheline.node
   * ('a, string) Tezos_micheline.Micheline.node
   * ('a, string) Tezos_micheline.Micheline.node list
and ('a, 'op, 'lit) args =
| Args_nil
| Args_cons of splitting * ('a, 'op, 'lit) expr * ('a, 'op, 'lit) args
and ('a, 'op, 'lit) binds =
| Binds of usage list * ('a, string) Tezos_micheline.Micheline.node list
   * ('a, 'op, 'lit) expr
and ('a, 'op, 'lit) cond =
| Cond of splitting * ('a, 'op, 'lit) expr * splitting * ('a, 'op, 'lit) binds
   * ('a, 'op, 'lit) binds
and ('a, 'op, 'lit) static_args =
| Type_args of string option * ('a, string) Tezos_micheline.Micheline.node list
| Script_arg of ('a, 'op, 'lit) script
and ('a, 'op, 'lit) script =
| Script of ('a, string) Tezos_micheline.Micheline.node
   * ('a, string) Tezos_micheline.Micheline.node * ('a, 'op, 'lit) binds

val binds_length : ('a1, 'a2, 'a3) binds -> nat

val args_length : ('a1, 'a2, 'a3) args -> nat
