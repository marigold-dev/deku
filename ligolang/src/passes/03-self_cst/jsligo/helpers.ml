[@@@warning "-26"]

open Cst.Jsligo

let npseq_to_ne_list (hd, tl) = hd, (List.map ~f:snd tl)

let map_npseq f (hd, tl) =
  let hd = f hd in
  let tl = List.map ~f:(fun (a,b) -> let b = f b in (a,b)) tl
  in (hd,tl)

let map_nseq f (hd, tl) =
  let hd = f hd in
  let tl = List.map ~f:(fun a -> let a = f a in a) tl
  in (hd,tl)

let fold_npseq f init (hd,tl) =
  let res = f init hd in
  let res = List.fold ~f:(fun init (_,b) -> f init b) ~init:res tl
  in res

let fold_nseq f init (hd,tl) =
  let res = f init hd in
  let res = List.fold ~f:(fun init b -> f init b) ~init:res tl
  in res

type ('a, 'err) folder = {
  e : 'a -> expr -> 'a;
  t : 'a -> type_expr -> 'a ;
  d : 'a -> statement -> 'a;
}

let rec fold_type_expression : ('a, 'err) folder -> 'a -> type_expr -> 'a =
  fun f init t ->
  let self = fold_type_expression f in
  let self_variant = fold_variant f in
  let init = f.t init t in
  match t with
    TProd  {inside = {value = { inside ;_};_} ; _} ->
      List.Ne.fold_left self init @@ npseq_to_ne_list inside
  | TSum {value;region=_} ->
     let {variants; attributes=_} = value in
     List.Ne.fold_left self_variant init @@ npseq_to_ne_list variants.value

  | TObject {value;region=_} ->
    let aux init ({value;region=_} : _ reg) =
      let {field_name=_;colon=_;field_type;attributes=_} = value in
      self init field_type
    in
    List.Ne.fold_left aux init @@ npseq_to_ne_list value.ne_elements
  | TApp    {value;region=_} ->
    let (_, tuple) = value in
    List.Ne.fold_left self init @@ npseq_to_ne_list tuple.value.inside
  | TFun    {value;region=_} ->
    let (ty1, _, ty2) = value in
    let fold_function_arg init ({type_expr; _}: fun_type_arg) = self init type_expr in
    let res = List.Ne.fold_left fold_function_arg init @@ npseq_to_ne_list ty1.inside in
    let res = self res  ty2 in
    res
  | TPar    {value;region=_} ->
    self init value.inside
  | TVar    _
  | TModA _
  | TInt _
  | TString _ -> init

and fold_variant : ('a, 'err) folder -> 'a -> variant reg -> 'a =
  fun f init v ->
    let self_type = fold_type_expression f in
    let component = v.value.tuple.value.inside in
    let {params; _} = component in
    match params with 
       Some params -> List.Ne.fold_left self_type init (npseq_to_ne_list (snd params))
    | None         -> init

let rec fold_expression : ('a, 'err) folder -> 'a -> expr -> 'a = fun f init e  ->
  let self = fold_expression f in
  let self_type = fold_type_expression f in
  let self_statement = fold_statement f in
  let self_module = fold_module f in
  let init = f.e init e in
  let bin_op value =
    let {op=_;arg1;arg2} = value in
    let res = fold_expression f init arg1 in
    let res = fold_expression f res  arg2 in
    res
  in
  match e with
    EAnnot   {value;region=_} ->
    let (expr, _, type_expr) = value in
    let res = self init expr in
    let res = self_type res type_expr in
    res
  | ELogic BoolExpr Or  {value;region=_} -> bin_op value
  | ELogic BoolExpr And {value;region=_} -> bin_op value
  | ELogic BoolExpr Not {value;region=_} ->
    let {op=_;arg} = value in
    let res = fold_expression f init arg in
    res
  | ELogic CompExpr Lt    {value;region=_}
  | ELogic CompExpr Leq   {value;region=_}
  | ELogic CompExpr Gt    {value;region=_}
  | ELogic CompExpr Geq   {value;region=_}
  | ELogic CompExpr Equal {value;region=_}
  | ELogic CompExpr Neq   {value;region=_} ->
    bin_op value
  | EArith Add   {value;region=_}
  | EArith Sub   {value;region=_}
  | EArith Mult  {value;region=_}
  | EArith Div   {value;region=_}
  | EArith Mod   {value;region=_} ->
    bin_op value
  | EArith Neg   {value;region=_} ->
    let {op=_;arg} = value in
    let res = fold_expression f init arg in
    res
  | EArith Int _ -> init
  | EString String   _
  | EString Verbatim _
  | EModA _
  | EVar _ -> init
  | EObject  {value;region=_} ->
    let aux init = function
      Punned_property {value; _} -> self init value
    | Property {value; _} ->
      let {name; value; _} = value in
      let res = self init name in
      let res = self res value in
      self res name
    | Property_rest {value; _} ->
      let ({expr; _}: property_rest) = value in
      self init expr
    in
    List.Ne.fold_left aux init @@ npseq_to_ne_list value.inside
  | EProj    {value; _} ->
    let {expr; selection} = value in
    let fold_selection init = function
      FieldName _ -> init
    | Component {value = {inside; _}; _} ->
      let res = self init inside in
      res
    in
    let res = self init expr in
    let res = fold_selection res selection in
    res
  | ECall    {value;region=_} ->
    let (lam, args) = value in
    let res = self init lam in
    (match args with
    | Unit _ -> res
    | Multiple {value;region=_} ->
      List.Ne.fold_left self res @@ npseq_to_ne_list value.inside
    )
  | EBytes   _ -> init
  | EUnit    _ -> init
  | EPar     {value;region=_} ->
    self init value.inside
  | EFun     {value;region=_} ->
    let {parameters; lhs_type; arrow=_; body} = value in
    let res = self init parameters in
    let res =
    (match lhs_type with
      Some (_, ty) -> self_type res ty
    | None ->    res
    ) in
    (match body with
      FunctionBody {value = {inside; _}; _} ->
        fold_npseq self_statement res inside
    | ExpressionBody e -> self res e
    )
  | ESeq     {value;region=_} ->
    fold_npseq self init value
  | ECodeInj _ ->
    init
  | EArray {value = {inside; _}; _} ->
    let fold_array_item init = function
      Expr_entry e -> self init e
    | Rest_entry {value = {expr; _}; _} -> self init expr
    in
    (match inside with 
      Some inside ->
        fold_npseq fold_array_item init inside
    | None -> init)
  | EAssign     (e1, _, e2) ->
    let res = self init e1 in
    let res = self res e2 in
    self res e1
  | EConstr {value;region=_} ->
    let _, expr = value in
    (match expr with
      None -> init
    | Some e -> self init e
    )

and fold_statement : ('a, 'err) folder -> 'a -> statement -> 'a =
  fun f init d ->
  let self_expr = fold_expression f in
  let self_type = fold_type_expression f in
  let self_module = fold_module f in
  let self = fold_statement f in
  let init = f.d init d in
  match d with
    SBlock {value = {inside; _}; _} -> fold_npseq self init inside
  | SExpr e -> self_expr init e
  | SCond {value = {test; ifso; ifnot}; _} ->
    let res = self_expr init test.inside in
    let res = self res ifso in
    (match ifnot with
      | None -> res
      | Some (_,e) -> self res e
      )
  | SReturn {value = {expr; _}; _} ->
    (match expr with
    | None -> init
    | Some e -> self_expr init e
    )
  | SLet {value = {bindings; _}; _}
  | SConst {value = {bindings; _}; _} ->
    let fold_binding init ({value; _}: val_binding reg) =
      let {lhs_type; expr; _} = value in
      let res = (match lhs_type with
      | None -> init
      | Some (_, t) -> self_type init t
      ) in
      self_expr res expr
    in
    fold_npseq fold_binding init bindings
  | SType {value; _} -> self_type init value.type_expr
  | SSwitch {value = {expr; cases; _}; _} ->
    let res = self_expr init expr in
    let fold_case init = function
      Switch_case {expr; statements; _} ->
        let res = self_expr init expr in
        (match statements with
          None -> res
        | Some s -> fold_npseq self res s
        )
    | Switch_default_case {statements; _} ->
      (match statements with
          None -> res
        | Some s -> fold_npseq self res s
        )
    in
    List.Ne.fold_left fold_case res cases
  | SBreak _ -> init
  | SNamespace {value = (_, _, {value = {inside; _}; _}, _ ); _} -> fold_npseq self init inside
  | SExport {value = (_, s); _} -> self init s
  | SImport _ -> init
  | SForOf {value = {expr; statement; _}; _}
  | SWhile {value = {expr; statement; _}; _} ->
      let res = self_expr init expr in
      self res statement

and remove_directives : toplevel_statements -> statement list =
fun (first, rest) ->
  let app top acc =
    match top with
      TopLevel (stmt, _) -> stmt::acc
    | Directive _ -> acc in
  List.fold_right ~f:app (first::rest) ~init:[]

and fold_module : ('a, 'err) folder -> 'a -> t -> 'a =
  fun f init {statements; _} ->
    let stmts = remove_directives statements in
    List.fold ~f:(fold_statement f) ~init stmts

type 'err mapper = {
  e : expr -> expr;
  t : type_expr -> type_expr ;
  d : statement -> statement ;
}

let rec map_type_expression : ('err) mapper -> type_expr -> 'b = fun f t ->
  let self = map_type_expression f in
  let self_variant = map_variant f in
  let t = f.t t in
  let return a = a in
  match t with
    TProd {inside = {value; region}; attributes} ->
    let inside = map_npseq self value.inside in
    let value = {value with inside} in
    return @@ TProd {inside = {value; region}; attributes}
  | TSum {value;region} ->
    let v     = map_npseq self_variant value.variants.value in
    let value = {value with variants = {value.variants with value=v}} in
    return @@ TSum {value; region}
  | TObject {value;region} ->
    let aux (element : _ reg ) =
      let field_type = self element.value.field_type in
      let value = {element.value with field_type} in
      {element with value }
    in
    let ne_elements = map_npseq aux value.ne_elements in
    let value = {value with ne_elements} in
    return @@ TObject {value;region}
  | TApp    {value;region} ->
    let (const, tuple) = value in
    let inside = map_npseq self tuple.value.inside in
    let tuple = {tuple with value = {tuple.value with inside }} in
    let value = (const, tuple) in
    return @@ TApp {value;region}
  | TFun {value; region} ->
    let map_fun_type_arg (f: fun_type_arg) =
      let type_expr = self f.type_expr in
      {f with type_expr}
    in
    let (ty1, wild, ty2) = value in
    let ty1 =
      let inside = map_npseq map_fun_type_arg ty1.inside in
      {lpar = ty1.lpar; inside; rpar = ty1.rpar}
    in
    let ty2 = self ty2 in
    let value = (ty1, wild, ty2) in
    return @@ TFun {value; region}
  | TPar    {value;region} ->
    let inside = self value.inside in
    let value = {value with inside} in
    return @@ TPar {value;region}
  | (TVar    _
  | TModA _
  | TInt _
  | TString _ as e) -> e

and map_variant : 'err mapper -> variant reg -> variant reg =
  fun f v -> 
    let self_type = map_type_expression f in
    let value = v.value in
    let tuple = value.tuple in
    let tuple_value = tuple.value in
    let inside = tuple_value.inside in
    let params = v.value.tuple.value.inside.params in
    let params = match params with 
      Some (comma, params) ->
        Some (comma, map_npseq self_type params)
    | None -> params
    in
    let inside = {inside with params} in
    let tuple_value = {tuple_value with inside} in
    let tuple = {tuple with value = tuple_value} in
    let value = {value with tuple} in
    let variant_reg = {v with value} in
    variant_reg

let rec map_expression : 'err mapper -> expr -> expr = fun f e  ->
  let self = map_expression f in
  let self_type = map_type_expression f in
  let self_module = map_module f in
  let self_statement = map_statement f in
  let return a = a in
  let e = f.e e in
  let bin_op value =
    let {op;arg1;arg2} = value in
    let arg1 = self arg1 in
    let arg2 = self arg2 in
    {op;arg1;arg2}
  in
  match e with
    EFun {value; region} ->
      let map_fun_expr_body = function
        FunctionBody {value; region} ->
          let inside = map_npseq self_statement value.inside in
          FunctionBody {
            value = {value with inside};
            region
          }
      | ExpressionBody e ->
        let e = self e in
        ExpressionBody e
      in
      let parameters = self value.parameters in
      let map_lhs_type (c, t) =
        let t = self_type t in
        (c, t)
      in
      let lhs_type = Option.map ~f:map_lhs_type value.lhs_type in
      let body = map_fun_expr_body value.body in
      let value = {
        parameters;
        lhs_type;
        arrow      = value.arrow;
        body
      } in
      return @@ EFun { value; region }
  | EPar {value; region} ->
      let inside = self value.inside in
      let value = {value with inside} in
      return @@ EPar { value; region }
  | ESeq  {value; region} ->
      let value = map_npseq self value in
      return @@ ESeq { value; region }
  | EVar v -> return @@ EVar v
  | EModA a -> return @@ EModA a
  | ELogic BoolExpr Or  {value;region} ->
    let value = bin_op value in
    return @@ ELogic (BoolExpr (Or {value;region}))
  | ELogic BoolExpr And {value;region} ->
    let value = bin_op value in
    return @@ ELogic (BoolExpr (And {value;region}))
  | ELogic BoolExpr Not {value;region} ->
    let arg = self value.arg in
    let value = {value with arg} in
    return @@ ELogic (BoolExpr (Not {value;region}))
  | ELogic CompExpr Lt    {value;region} ->
    let value = bin_op value in
    return @@ ELogic (CompExpr (Lt {value;region}))
  | ELogic CompExpr Leq   {value;region} ->
    let value = bin_op value in
    return @@ ELogic (CompExpr (Leq {value;region}))
  | ELogic CompExpr Gt    {value;region} ->
    let value = bin_op value in
    return @@ ELogic (CompExpr (Gt {value;region}))
  | ELogic CompExpr Geq   {value;region} ->
    let value = bin_op value in
    return @@ ELogic (CompExpr (Geq {value;region}))
  | ELogic CompExpr Equal {value;region} ->
    let value = bin_op value in
    return @@ ELogic (CompExpr (Equal {value;region}))
  | ELogic CompExpr Neq   {value;region} ->
    let value = bin_op value in
    return @@ ELogic (CompExpr (Neq {value;region}))
  | EArith Add   {value;region} ->
    let value = bin_op value in
    return @@ EArith (Add {value;region})
  | EArith Sub   {value;region} ->
    let value = bin_op value in
    return @@ EArith (Sub {value;region})
  | EArith Mult  {value;region} ->
    let value = bin_op value in
    return @@ EArith (Mult {value;region})
  | EArith Div   {value;region} ->
    let value = bin_op value in
    return @@ EArith (Div {value;region})
  | EArith Mod   {value;region} ->
    let value = bin_op value in
    return @@ EArith (Mod {value;region})
  | EArith Neg   {value;region} ->
    let arg = self value.arg in
    let value = {value with arg} in
    return @@ EArith (Neg {value;region})
  | EArith Int   _ -> return @@ e
  (* | EArith Nat   _ *)
  (* | EArith Mutez _ as e  *)
  | ECall {value = (e, a); region} ->
    let map_arguments = function
      Multiple {value; region} ->
        let inside = map_npseq self value.inside in
        let value = {value with inside} in
        Multiple { value; region }
    | Unit _ as u -> u
    in
    let e = self e in
    let a = map_arguments a in
    let value = (e, a) in
    return @@ ECall {value; region}
  | EBytes _ as e -> return @@ e
  | EArray {value;region} ->
      let map_array_item = function
        Expr_entry e ->
        let e = self e in
        Expr_entry e
      | Rest_entry {value; region} ->
        let expr = self value.expr in
        Rest_entry {value = {value with expr}; region}
      in
      let inside = (match value.inside with 
        Some inside ->
          Some (map_npseq map_array_item inside)
      | None ->
          None
      )
      in
      let value = {value with inside} in
      return @@ EArray {value; region}
  | EObject {value;region} ->
    let map_property = function
      Punned_property {value; region} ->
        let value = self value in
        Punned_property {value; region}
    | Property {value; region} ->
      let name = self value.name in
      let value2 = self value.value in
      Property {value = {value with name; value = value2}; region}
    | Property_rest {value; region} ->
      let expr = self value.expr in
      Property_rest {value = {value with expr}; region}
    in
    let inside = map_npseq map_property value.inside in
    let value = {value with inside} in
    return @@ EObject {value;region}
  | EString _ as e -> return @@ e
  | EProj {value; region} ->
    let map_selection = function
      FieldName _ as f -> f
    | Component {value; region} ->
      let inside = self value.inside in
      Component {
        value = {value with inside};
        region
      }
    in
    let expr = self value.expr in
    let selection = map_selection value.selection in
    let value = {expr;selection} in
    return @@ EProj {value; region}
  | EAssign     (a, e, b) ->
    let a = self a in
    let b = self b in
    return @@ EAssign (a, e, b)
  | EAnnot {value; region} ->
    let (a, e, b) = value in
    let a = self a in
    let b = self_type b in
    return @@ EAnnot {value = (a, e, b); region}
  | EUnit _ as u -> return @@ u
  | ECodeInj {value;region} ->
    let code = self value.code in
    let value = {value with code} in
    return @@ ECodeInj {value;region}
  | EConstr {value;region} ->
    let const, expr = value in
    let expr = Option.map ~f:self expr in
    let value = const,expr in
    return @@ EConstr {value;region}

and map_statement : ('err) mapper -> statement -> statement =
  fun f s ->
  let self = map_statement f in
  let self_expr = map_expression f in
  let self_type = map_type_expression f in
  let self_module = map_module f in
  let return a = a in
  (* let s = f.d s in *)
  match s with
    SBlock {value; region} ->
      let inside = map_npseq self value.inside in
      return @@ SBlock {
        value = {value with inside};
        region
      }
  | SExpr e ->
    let e = self_expr e in
    return @@ SExpr e
  | SCond {value; region} ->
    let {kwd_if;test = {inside; lpar; rpar};ifso;ifnot} = value in
      let inside = self_expr inside in
      let ifso = self ifso in
      let map_ifnot (else_, statement) =
        let statement = self statement in
        (else_, statement)
      in
      let ifnot = Option.map ~f:map_ifnot ifnot in
      let value = {kwd_if;test = {inside; lpar;rpar};ifso;ifnot} in
      return @@ SCond {value;region}
  | SReturn {value = {kwd_return; expr}; region} ->
    let expr = Option.map ~f:self_expr expr in
    return @@ SReturn {value = {
      kwd_return;
      expr;
      };
      region
    }
  | SLet {value; region} ->
      let map_lhs_type (c, t) =
        let t = self_type t in
        (c, t)
      in
      let map_binding ({value; region}: val_binding Region.reg) =
        let a = region in
        let lhs_type = Option.map ~f:map_lhs_type value.lhs_type in
        let expr = self_expr value.expr in
        ({value = {value with lhs_type; expr}; region}: val_binding Region.reg)
      in
      let bindings = map_npseq map_binding value.bindings in
      return @@ SLet {
        value = {value with bindings};
        region
      }
  | SConst {value; region} ->
    let map_lhs_type (c, t) =
      let t = self_type t in
      (c, t)
    in
    let map_binding ({value; region}: val_binding Region.reg) =
      let a = region in
      let lhs_type = Option.map ~f:map_lhs_type value.lhs_type in
      let expr = self_expr value.expr in
      ({value = {value with lhs_type; expr}; region}: val_binding Region.reg)
    in
    let bindings = map_npseq map_binding value.bindings in
    return @@ SConst {
      value = {value with bindings};
      region
    }
  | SType   {value; region} ->
    let type_expr = self_type value.type_expr in
    return @@ SType { value = {value with type_expr}; region }
  | SSwitch {value; region} ->
    let map_case = function
      Switch_case c ->
        let expr = self_expr c.expr in
        let statements = Option.map ~f:(map_npseq self) c.statements in
        Switch_case {c with expr; statements}
    | Switch_default_case d ->
      let statements = Option.map ~f:(map_npseq self) d.statements in
      Switch_default_case {d with statements}
    in
    let expr = self_expr value.expr in
    let cases = List.Ne.map map_case value.cases in
    return @@ SSwitch { value = {value with expr; cases}; region}
  | SBreak b ->
    return @@ SBreak b
  | SNamespace {value; region} ->
    let (kwd_namespace, name, statements, attributes) = value in
    let ({value = statements_value; region = statements_region}: statements braces reg) = statements in
    let inside = map_npseq self statements_value.inside in
    let statements: statements braces reg = {
      value = {
        statements_value with
        inside
      };
      region = statements_region
    } in
    let value = (kwd_namespace, name, statements, attributes) in
    return @@ SNamespace {
      value;
      region
    }
  | SExport {value; region} ->
    let (kwd_export, statement) = value in
    let statement = self statement in
    return @@ SExport {
      value = (kwd_export, statement);
      region
    }
  | SImport i -> return @@ SImport i
  | SForOf {value; region} -> (
    let expr = self_expr value.expr in
    let statement = self value.statement in
    return @@ SForOf {
      value = {value with expr; statement };
      region
    }
  )
  | SWhile {value; region} -> (
    let expr = self_expr value.expr in
    let statement = self value.statement in
    return @@ SWhile {
      value = {value with expr; statement };
      region
    }
  )

and map_toplevel_statement f = function
  TopLevel (statement, terminator) ->
    let stmt = map_statement f statement
    in TopLevel (stmt, terminator)
  | Directive _ as d -> d

and map_module : 'err mapper -> t -> t =
  fun f {statements; eof} ->
    let self = map_toplevel_statement f in
    (fun statements -> {statements; eof})
    @@ map_nseq self statements

(* TODO this is stupid *)
let fold_to_map : unit -> (unit, 'err) folder -> ('err) mapper =
  fun init {e;t;d} ->
  let e expr =
    let () = e init expr in expr
  in
  let t ty =
    let () = t init ty in ty
  in
  let d decl =
    let () = d init decl in decl
  in
  {e;t;d}
