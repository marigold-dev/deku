[@@@coverage exclude_file]
open Simple_utils.PP_helpers
open Types
open Format

let list_sep_d x = list_sep x (tag " ,@ ")

let rec type_variable ppf : type_expression -> _ = fun te -> match te.type_content with
  | T_or(a, b) -> fprintf ppf "@[(%a) |@ (%a)@]" annotated a annotated b
  | T_tuple ts ->
    fprintf ppf "@[(%a)@]" (list_sep annotated (tag " *@ ")) ts
  | T_base b -> type_constant ppf b
  | T_function(a, b) -> fprintf ppf "@[(%a) ->@ (%a)@]" type_variable a type_variable b
  | T_map(k, v) -> fprintf ppf "@[<4>map(%a -> %a)@]" type_variable k type_variable v
  | T_big_map(k, v) -> fprintf ppf "@[<9>big_map(%a -> %a)@]" type_variable k type_variable v
  | T_list(t) -> fprintf ppf "@[<5>list(%a)@]" type_variable t
  | T_set(t) -> fprintf ppf "@[<4>set(%a)@]" type_variable t
  | T_option(o) -> fprintf ppf "@[<7>option(%a)@]" type_variable o
  | T_contract(t) -> fprintf ppf "@[<9>contract(%a)@]" type_variable t
  | T_sapling_transaction t -> fprintf ppf "Sapling_transaction (%d)" (Z.to_int t)
  | T_sapling_state t -> fprintf ppf "Sapling_state (%d)" (Z.to_int t)
  | T_ticket t -> fprintf ppf "Ticket (%a)" type_variable t

and annotated ppf : type_expression annotated -> _ = function
  | (Some ann, a) -> fprintf ppf "(%a %%%s)" type_variable a ann
  | (None, a) -> type_variable ppf a

and environment_element ppf ((n, tv) : environment_element) =
  Format.fprintf ppf "%a : %a" Var.pp n.wrap_content type_variable tv

and environment ppf (x:environment) =
  fprintf ppf "Env[%a]" (list_sep_d environment_element) x

and type_constant ppf (tb:type_base) : unit =
  let s = match tb with
    | TB_unit      -> "unit"
    | TB_string    -> "string"
    | TB_bytes     -> "bytes"
    | TB_nat       -> "nat"
    | TB_int       -> "int"
    | TB_mutez     -> "mutez"
    | TB_bool      -> "bool"
    | TB_operation -> "operation"
    | TB_address   -> "address"
    | TB_key       -> "key"
    | TB_key_hash  -> "key_hash"
    | TB_signature -> "signature"
    | TB_timestamp -> "timestamp"
    | TB_chain_id  -> "chain_id"
    | TB_baker_hash          -> "baker_hash"
    | TB_pvss_key            -> "pvss_key"
    | TB_baker_operation     -> "baker_operation"
    | TB_bls12_381_g1        -> "bls12_381_g1"
    | TB_bls12_381_g2        -> "bls12_381_g2"
    | TB_bls12_381_fr        -> "bls12_381_fr"
    | TB_never -> "never"
    | TB_chest -> "chest"
    | TB_chest_key -> "chest_key"
    in
  fprintf ppf "%s" s

let rec value ppf : value -> unit = function
  | D_bool b -> fprintf ppf "%b" b
  | D_operation _ -> fprintf ppf "operation[...bytes]"
  | D_int n -> fprintf ppf "%a" Z.pp_print n
  | D_nat n -> fprintf ppf "+%a" Z.pp_print n
  | D_timestamp n -> fprintf ppf "+%a" Z.pp_print n
  | D_mutez n -> fprintf ppf "%amutez" Z.pp_print n
  | D_unit -> fprintf ppf "unit"
  | D_string s -> fprintf ppf "\"%s\"" s
  | D_bytes x ->
    fprintf ppf "0x%a" Hex.pp @@ Hex.of_bytes x
  | D_pair (a, b) -> fprintf ppf "(%a), (%a)" value a value b
  | D_left a -> fprintf ppf "L(%a)" value a
  | D_right b -> fprintf ppf "R(%a)" value b
  | D_none -> fprintf ppf "None"
  | D_some s -> fprintf ppf "Some (%a)" value s
  | D_map m -> fprintf ppf "Map[%a]" (list_sep_d value_assoc) m
  | D_big_map m -> fprintf ppf "Big_map[%a]" (list_sep_d value_assoc) m
  | D_list lst -> fprintf ppf "List[%a]" (list_sep_d value) lst
  | D_set lst -> fprintf ppf "Set[%a]" (list_sep_d value) lst
  | D_ticket (a,b) -> fprintf ppf "ticket(%a,%a)" value a value b

and type_expression_annotated ppf : type_expression annotated -> unit = fun (_, tv) ->
  type_expression ppf tv

and type_expression ppf : type_expression -> unit = fun te ->
  fprintf ppf "%a" type_content te.type_content
  
and type_content ppf : type_content -> unit = function
  | T_tuple ts ->
    fprintf ppf "@[(%a)@]" (list_sep annotated (tag " *@ ")) ts
  | T_or    (a,b) -> fprintf ppf "or %a %a" type_expression_annotated a type_expression_annotated b
  | T_function (a, b) -> fprintf ppf "lambda (%a) %a" type_expression a type_expression b
  | T_base tc -> fprintf ppf "%a" type_constant tc
  | T_map (k,v) -> fprintf ppf "Map (%a,%a)" type_expression k type_expression v
  | T_big_map (k,v) -> fprintf ppf "Big_map (%a,%a)" type_expression k type_expression v
  | T_list e -> fprintf ppf "List (%a)" type_expression e
  | T_set e -> fprintf ppf "Set (%a)" type_expression e
  | T_contract c -> fprintf ppf "Contract (%a)" type_expression c
  | T_option c -> fprintf ppf "Option (%a)" type_expression c
  | T_sapling_transaction x -> fprintf ppf "Sapling_transaction (%d)" (Z.to_int x)
  | T_sapling_state x -> fprintf ppf "Sapling_state (%d)" (Z.to_int x)
  | T_ticket x -> fprintf ppf "Ticket (%a)" type_expression x

and value_assoc ppf : (value * value) -> unit = fun (a, b) ->
  fprintf ppf "%a -> %a" value a value b

and expression ppf (e:expression) =
  fprintf ppf "%a" expression_content e.content

and expression_content ppf (e:expression_content) = match e with
  | E_closure x -> function_ ppf x
  | E_variable v -> fprintf ppf "%a" Var.pp v.wrap_content
  | E_application(a, b) -> fprintf ppf "@[(%a)@(%a)@]" expression a expression b

  | E_constant c -> fprintf ppf "@[%a@[<hv 1>(%a)@]@]" constant c.cons_name (list_sep_d expression) c.arguments
  | E_literal v -> fprintf ppf "@[L(%a)@]" Stage_common.PP.literal v
  | E_if_bool (c, a, b) ->
    fprintf ppf
      "@[match %a with@ @[<hv>| True ->@;<1 2>%a@ | False ->@;<1 2>%a@]@]"
      expression c expression a expression b
  | E_if_none (c, n, ((name, _) , s)) ->
    fprintf ppf
      "@[match %a with@ @[<hv>| None ->@;<1 2>%a@ | Some %a ->@;<1 2>%a@]@]"
      expression c expression n Var.pp name.wrap_content expression s
  | E_if_cons (c, n, (((hd_name, _) , (tl_name, _)) , cons)) ->
    fprintf ppf "@[%a ?? %a : (%a :: %a) -> %a@]"
      expression c expression n Var.pp hd_name.wrap_content Var.pp tl_name.wrap_content expression cons
  | E_if_left (c, ((name_l, _) , l), ((name_r, _) , r)) ->
      fprintf ppf
        "@[match %a with@ @[<hv>| Left %a ->@;<1 2>%a@ | Right %a ->@;<1 2>%a@]@]"
        expression c Var.pp name_l.wrap_content expression l Var.pp name_r.wrap_content expression r
  | E_let_in (expr, inline , ((name , _) , body)) ->
      fprintf ppf "@[let %a =@;<1 2>%a%a in@ %a@]" Var.pp name.wrap_content expression expr option_inline inline expression body
  | E_tuple exprs ->
    fprintf ppf "@[(%a)@]"
      Format.(pp_print_list ~pp_sep:(const ", ") expression)
      exprs
  | E_let_tuple (expr, (fields, body)) ->
      fprintf ppf "@[let (%a) =@;<1 2>%a in@ %a@]"
        Format.(pp_print_list ~pp_sep:(fun ppf () -> pp_print_string ppf ", ") Var.pp)
          (List.map ~f:(fun (x, _) -> x.Location.wrap_content) fields)
        expression expr
        expression body
  | E_proj (expr, i, _n) ->
      fprintf ppf "(%a).(%d)" expression expr i
  | E_update (expr, i, update, _n) ->
      fprintf ppf "{ %a with (%d) = %a }"
        expression expr i expression update
  | E_iterator (b , ((name , _) , body) , expr) ->
    fprintf ppf "@[for_%a %a of %a do ( %a )@]" constant b Var.pp name.wrap_content expression expr expression body
  | E_fold (((name , _) , body) , collection , initial) ->
    fprintf ppf "@[fold %a on %a with %a do ( %a )@]" expression collection expression initial Var.pp name.wrap_content expression body
  | E_fold_right (((name , _) , body) , (collection,_) , initial) ->
    fprintf ppf "@[fold_right %a on %a with %a do ( %a )@]" expression collection expression initial Var.pp name.wrap_content expression body
  | E_raw_michelson code ->
    let open Tezos_micheline in
    let code = Micheline.Seq (Location.generated, code) in
    let code = Micheline.strip_locations code in
    let code = Micheline_printer.printable (fun prim -> prim) code in
    fprintf ppf "%a" Micheline_printer.print_expr code

and expression_with_type : _ -> expression -> _  = fun ppf e ->
  fprintf ppf "%a : %a"
    expression_content e.content
    type_variable e.type_expression

and function_ ppf ({binder ; body}:anon_function) =
  fprintf ppf "@[fun %a ->@ (%a)@]"
    Var.pp binder.wrap_content
    expression body

and option_inline ppf inline =
  if inline then
    fprintf ppf "[@@inline]"
  else
    fprintf ppf ""

and declaration ppf ((n,i, e):assignment) = fprintf ppf "@[let %a =@;<1 2>%a%a@]" Var.pp n.wrap_content expression e option_inline i

and tl_statement ppf (ass, _) = declaration ppf ass

and program ppf (p:program) =
  fprintf ppf "@[<v>%a@]" (pp_print_list ~pp_sep:(tag "@ ") tl_statement) p

and constant ppf : constant' -> unit = Stage_common.PP_enums.constant' ppf

let%expect_test _ =
  Format.printf "%a" value (D_bytes (Bytes.of_string "foo")) ;
  [%expect{| 0x666f6f |}]

let%expect_test _ =
  let pp = expression_content Format.std_formatter in
  let dummy_type = {type_content=T_base TB_unit;location=Location.generated} in
  let wrap e = { content = e ; type_expression = dummy_type ; location = Location.generated} in
  let y = Location.wrap ~loc:(Location.generated) (Var.of_name "y") in
  let z = Location.wrap ~loc:(Location.generated) (Var.of_name "z") in
  pp @@ E_closure { binder = y ; body = wrap (E_variable y) } ;
  [%expect{|
    fun y -> (y)
  |}] ;
  pp @@ E_closure { binder = z ; body = wrap (E_variable z) } ;
  [%expect{|
    fun z -> (z)
  |}]
