open Token

let semi = parser [< 'SEMI _ >] -> ()
let vbar = parser [< 'VBAR _ >] -> ()
let end_ = parser [< 'End _ >] -> ()
let rbracket = parser [< 'RBRACKET _ >] -> ()
let times = parser [< 'TIMES _ >] -> ()
let arrow = parser [< 'ARROW _ >] -> fun _ _ -> ()
let comma = parser [< 'COMMA _ >] -> ()
let rbrace = parser [< 'RBRACE _ >] -> ()
let equal = parser [< 'EQUAL _ >] -> ()
let ass = parser [< 'ASS _ >] -> ()
let dot = parser [< 'DOT _ >] -> ()
let down = parser [< 'Down _ >] -> ()
let or_ = parser [< 'Or _ >] -> fun _ _ -> ()
let and_ = parser [< 'And _ >] -> fun _ _ -> ()
let cat = parser [< 'CAT _ >] -> fun _ _ -> ()
let cons = parser [< 'CONS _ >] -> ()
let cons' = parser [< 'CONS _ >] -> fun _ _ -> ()
let list = parser [< 'List _ >] -> ()
let map = parser [< 'Map _ >] -> ()
let set = parser [< 'Set _ >] -> ()

let left_assoc item op =
  let rec op_elem i = parser
    [< f=op; j=item; r = op_elem (f i j) ?! >] -> r
  | [< >] -> i
  in parser [< i=item; r = op_elem i >] -> r

let rec right_assoc item op = parser
  [< i=item >] -> i
| [< i=item; f=op; j = right_assoc item op ?! >] -> f i j

let opt item = parser
  [< i=item >] -> Some i
| [< >]        -> None

let rec series item sep term = parser
  [< i=item; l = after_item item sep term >] -> i,l

and after_item item sep term = parser
  [< t=term >] ->
    [], None, t
| [< s=sep; io = item_or_closing item sep term >] ->
    match io with
      `Some (item, items, term, closing) ->
        (s, item)::items, term, closing
    | `Closing closing ->
        [], Some s, closing

and item_or_closing item sep term = parser
  [< t=term >] ->
    `Closing t
| [< s = series item sep term >] ->
    let item, (items, term, closing) = s
    in `Some (item, items, term, closing)

(* Compound constructs *)

let par item = parser
  [< 'LPAR _; _=item; 'RPAR _ >] -> ()

let brackets item = parser
  [< 'LBRACKET _; _=item; 'RBRACKET _ >] -> ()

(* Sequences *)

(* Possibly empty sequence of items *)

let rec seq item = parser
  [< h,t = nseq item >] -> h::t

(* Non-empty sequence of items *)

and nseq item = parser
  [< i=item; l = seq item >] -> i,l

(* Non-empty separated sequence of items *)

and nsepseq item sep = parser
  [< i=item >] -> i, []
| [< i=item; s=sep; h,t = nsepseq item sep ?! >] -> i, (s,h)::t

(* Possibly empty separated sequence of items *)
(*
and sepseq item sep = opt (nsepseq item sep)
*)

(* Main *)

let rec contract = parser
  [< _ = nseq declaration; 'EOF _ >] -> ()

and declaration = parser
  [< _ = type_decl; _ = opt semi >] -> ()
| [< _ = const_decl; _ = opt semi >] -> ()
| [< _ = lambda_decl; _ = opt semi >] -> ()

and type_decl = parser
  [< 'Type _; 'Ident _; 'Is _; _=type_expr >] -> ()

and type_expr = parser
  [< _=cartesian >] -> ()
| [< _=nsepseq variant vbar >] -> ()
| [< 'VBAR _; _ = nsepseq variant vbar >] -> ()
| [< 'Record _; _=record_type_expr >] -> ()

and record_type_expr = parser
  [< _ = series field_decl semi end_ >] -> ()
| [< 'LBRACKET _; _ = series field_decl semi rbracket >] -> ()

and variant = parser
  [< 'Constr _; _ = opt of_cartesian >] -> ()

and of_cartesian = parser
  [< 'Of _; _=cartesian >] -> ()

and cartesian = parser
  [< _ = nsepseq function_type times >] -> ()

and function_type strm = right_assoc core_type arrow strm

and core_type = parser
  [< 'Ident _; _ = opt type_tuple >] -> ()
| [< 'Map _; _=type_tuple >] -> ()
| [< 'Set _; _ = par type_expr >] -> ()
| [< 'List _; _ = par type_expr >] -> ()
| [< _ = par type_expr >] -> ()

and type_tuple = parser
  [< _ = par (nsepseq type_expr comma) >] -> ()

and field_decl = parser
  [< 'Ident _; 'COLON _; _=type_expr >] -> ()

(* Function and procedure declarations *)

and lambda_decl = parser
  [< _=fun_decl >] -> ()
| [< _=proc_decl >] -> ()

and fun_decl = parser
  [< 'Function _; 'Ident _; _=parameters; 'COLON _;
     _=type_expr; 'Is _; _ = seq local_decl; _=block;
     'With _; _=expr >] -> ()

and proc_decl = parser
  [< 'Procedure _; 'Ident _; _parameters; 'Is _;
     _ = seq local_decl; _=block >] -> ()

and parameters = parser
  [< p = par (nsepseq param_decl semi) >] -> p

and param_decl = parser
  [< 'Var _; 'Ident _; 'COLON _; _=param_type >] -> ()
| [< 'Const _; 'Ident _; 'COLON _; _=param_type >] -> ()

and param_type = parser [< c = cartesian >] -> c

and block = parser
  [< 'Begin _; _ = series statement semi end_ >] -> ()
| [< 'Block _; 'LBRACE _; _ = series statement semi rbrace >] -> ()

and statement = parser
  [< _=instruction >] -> ()
| [< _=data_decl >] -> ()

and data_decl = parser
  [< _=const_decl >] -> ()
| [< _=var_decl >] -> ()

and const_decl = parser
  [< 'Const _; _ = unqualified_decl equal >] -> ()

and var_decl = parser
  [< 'Var _; _ = unqualified_decl ass >] -> ()

and local_decl = parser
  [< _=fun_decl; _ = opt semi >] -> ()
| [< _=proc_decl; _ = opt semi >] -> ()
| [< _=data_decl;  _ = opt semi >] -> ()

and unqualified_decl op = parser
  [< 'Ident _; 'COLON _; _=type_expr; _=op; _=expr >] -> ()

and instruction = parser
  [< _=block >] -> ()
| [< 'If _; _=expr; 'Then _; _=if_clause; _ = opt semi;
     'Else _; _=if_clause >] -> ()
| [< _ = case instruction >] -> ()
| [< 'Ident _; _=instruction_1 >] -> ()
| [< _=loop >] -> ()
| [< 'Fail _; _=expr >] -> ()
| [< 'Skip _ >] -> ()
| [< 'Patch _; _=path; 'With _; _=structure >] -> ()
| [< 'Remove _; _=expr; 'From _; _=remove_suffix >] -> ()

and remove_suffix = parser
  [< 'Map _; _=path >] -> ()
| [< 'Set _; _=path >] -> ()

and instruction_1 = parser
  [< _=arguments >] -> ()
| [< 'ASS _; _=expr >] -> ()
| [< _ = brackets expr; 'ASS _; _=expr >] -> ()
| [< _=selections;
     _ = opt (brackets expr); 'ASS _; _=expr >] -> ()

and path = parser
  [< 'Ident _; _ = opt selections >] -> ()

and selections = parser
  [< 'DOT _; _ = nsepseq selection dot >] -> ()

and injection kind element = parser
  [< _=kind; _ = inj_suffix element >] -> ()

and inj_suffix element = parser
  [< _ = series element semi end_ >] -> ()
| [< 'End _ >] -> ()
| [< 'LBRACKET _; _ = bracketed element >] -> ()

and bracketed element = parser
  [< _ = series element semi rbracket >] -> ()
| [< 'RBRACKET _ >] -> ()

and binding = parser
  [< _=expr; 'ARROW _; _=expr >] -> ()

and if_clause = parser
  [< _=instruction >] -> ()
| [< 'LBRACE _; _ = series statement comma rbrace >] -> ()

and case rhs = parser
  [< 'Case _; _=expr; 'Of _; _ = case_suffix rhs >] -> ()

and case_suffix rhs = parser
  [< _ = cases rhs; 'End _ >] -> ()
| [< 'LBRACKET _; _ = cases rhs; 'RBRACKET _ >] -> ()

and cases rhs = parser
  [< _ = nsepseq (case_clause rhs) vbar >] -> ()
| [< 'VBAR _; _ = nsepseq (case_clause rhs) vbar >] -> ()

and case_clause rhs = parser
  [< _=pattern; 'ARROW _; _=rhs >] -> ()

and loop = parser
  [< 'While _; _=expr; _=block >] -> ()
| [< 'For _; 'Ident _; _=for_suffix >] -> ()

and for_suffix = parser
  [< 'ASS _; _=expr; _ = opt down;
     'To _; _=expr; _ = opt step_clause; _=block >] -> ()
| [< 'In _; _=expr; _=block >] -> ()
| [< 'ARROW _; 'Ident _; 'In _; _=expr; _=block >] -> ()

and step_clause = parser
  [< 'Step _; _=expr >] -> ()

(* Expressions *)

and interactive = parser
  [< _=expr; 'EOF _ >] -> ()

and expr = parser
  [< _ = case expr >] -> ()
| [< _=disj_expr >] -> ()

and disj_expr strm = left_assoc conj_expr or_ strm

and conj_expr strm = left_assoc set_membership and_ strm

and set_membership = parser
  [< _ = comp_expr; _ = opt contains_clause >] -> ()

and contains_clause = parser
  [< 'Contains _; _=set_membership >] -> ()

and comp_expr strm = left_assoc cat_expr op_comp strm

and op_comp = parser
  [< 'LT _ >] -> fun _ _ -> ()
| [< 'LEQ _ >] -> fun _ _ -> ()
| [< 'GT _ >] -> fun _ _ -> ()
| [< 'GEQ _ >] -> fun _ _ -> ()
| [< 'EQUAL _ >] -> fun _ _ -> ()
| [< 'NEQ _ >] -> fun _ _ -> ()

and cat_expr strm = right_assoc cons_expr cat strm

and cons_expr strm = left_assoc add_expr cons' strm

and add_expr strm = left_assoc mult_expr add_op strm

and add_op = parser
  [< 'PLUS _ >] -> fun _ _ -> ()
| [< 'MINUS _ >] -> fun _ _ -> ()

and mult_expr strm = left_assoc unary_expr mult_op strm

and mult_op = parser
  [< 'TIMES _ >] -> fun _ _ -> ()
| [< 'SLASH _ >] -> fun _ _ -> ()
| [< 'Mod _ >] -> fun _ _ -> ()

and unary_expr = parser
  [< 'MINUS _; _=core_expr >] -> ()
| [< 'Not _; _=core_expr >] -> ()
| [< _=core_expr >] -> ()

and core_expr = parser
  [< 'Int _ >] -> ()
| [< 'Nat _ >] -> ()
| [< 'Mutez _ >] -> ()
| [< 'Ident _; _ = opt core_suffix >] -> ()
| [< 'String _ >] -> ()
| [< 'Bytes _ >] -> ()
| [< 'C_False _ >] -> ()
| [< 'C_True _ >] -> ()
| [< 'C_Unit _ >] -> ()
| [< 'C_None _ >] -> ()
| [< 'C_Some _; _=arguments >] -> ()
| [< 'Constr _; _ = opt arguments >] -> ()
| [< _ = par paren_expr >] -> ()
| [< _ = injection list expr >] -> ()
| [< 'Nil _ >] -> ()
| [< _=structure >] -> ()

and core_suffix = parser
  [< _ = brackets expr >] -> ()
| [< 'DOT _; _ = nsepseq selection dot;
     _ = opt (brackets expr) >] -> ()
| [< _=arguments >] -> ()

and paren_expr = parser
  [< _=disj_expr; _ = opt paren_expr_1 >] -> ()
| [< _ = case expr; _ = opt paren_expr_2 >] -> ()

and paren_expr_1 = parser
  [< 'COLON _; _=type_expr >] -> ()
| [< 'COMMA _; _ = nsepseq expr comma >] -> ()

and paren_expr_2 = parser
  [< 'COMMA _; _ = nsepseq expr comma >] -> ()

and structure = parser
  [< _ = injection map binding >] -> ()
| [< _ = injection set expr >] -> ()
| [< _=record_expr >] -> ()

and selection = parser
  [< 'Ident _ >] -> ()
| [< 'Int _ >] -> ()

and record_expr = parser
  [< 'Record _; _=record_expr_suffix >] -> ()

and record_expr_suffix = parser
  [< _ = series field_assignment semi end_ >] -> ()
| [< 'LBRACKET _;
     _ = series field_assignment semi rbracket >] -> ()

and field_assignment = parser
  [< 'Ident _; 'EQUAL _; _=expr >] -> ()

and arguments = parser
  [< _ = par (nsepseq expr comma) >] -> ()

(* Patterns *)

and pattern = parser
  [< _ = nsepseq core_pattern cons >] -> ()

and core_pattern = parser
  [< 'Ident _ >] -> ()
| [< 'WILD _ >] -> ()
| [< 'Int _ >] -> ()
| [< 'String _ >] -> ()
| [< 'C_Unit _ >] -> ()
| [< 'C_False _ >] -> ()
| [< 'C_True _ >] -> ()
| [< 'C_None _ >] -> ()
| [< 'C_Some _; _ = par core_pattern >] -> ()
| [< 'Constr _; _ = opt tuple_pattern >] -> ()
| [< _ = injection list core_pattern >] -> ()
| [< 'Nil _ >] -> ()
| [< _ = par (opt paren_pattern) >] -> ()

and paren_pattern = parser
  [< 'CONS _; _=pattern >] -> ()
| [< 'COMMA _; _ = nsepseq core_pattern comma >] -> ()

and tuple_pattern = parser
  [< _ = par (nsepseq core_pattern comma) >] -> ()
