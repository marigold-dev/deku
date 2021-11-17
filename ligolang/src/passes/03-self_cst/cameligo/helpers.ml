open Cst.Cameligo
open Simple_utils

let nseq_to_list (hd, tl) = hd :: tl

let npseq_to_list (hd, tl) = hd :: (List.map ~f:snd tl)

let npseq_to_ne_list (hd, tl) = hd, (List.map ~f:snd tl)
let map_npseq f (hd,tl) = f hd, List.map ~f:(fun (a,b) -> (a, f b)) tl
let fold_npseq f init (hd,tl) =
  let res = f init hd in
  let res = List.fold ~f:(fun init (_,b) -> f init b) ~init:res tl in
  res

let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst
let map_pseq f = Option.map ~f:(map_npseq f)
let fold_pseq f init seq =
  let res = Option.map ~f:(fold_npseq f init) seq in
  Option.value ~default:(init) res

type 'a folder = {
  e : 'a -> expr -> 'a;
  t : 'a -> type_expr -> 'a;
  d : 'a -> declaration -> 'a;
}

let rec fold_type_expression : 'a folder -> 'a -> type_expr -> 'a = fun f init t ->
  let self = fold_type_expression f in
  let init = f.t init t in
  match t with
    TProd   {value;region=_} ->
    List.Ne.fold_left self init @@ npseq_to_ne_list value
  | TSum    {value;region=_} ->
    let {lead_vbar=_;variants;attributes=_} = value in
    let aux init ({value;region=_} : _ reg) =
      let {constr=_;arg;attributes=_} = value in
      match arg with
        Some (_,t) -> self init t
      | None -> init
    in
    List.Ne.fold_left aux init @@ npseq_to_ne_list variants
  | TRecord {value;region=_} ->
    let aux init ({value;region=_} : _ reg) =
      let {field_name=_;colon=_;field_type;attributes=_} = value in
      self init field_type
    in
    List.Ne.fold_left aux init @@ npseq_to_ne_list value.ne_elements
  | TApp    {value;region=_} -> (
    let (_, args) = value in
    match args with
    | CArgTuple x -> List.Ne.fold_left self init @@ npseq_to_ne_list x.value.inside
    | CArg x -> self init x
  )
  | TFun    {value;region=_} ->
    let (ty1, _, ty2) = value in
    let res = self init ty1 in
    let res = self res  ty2 in
    res
  | TPar    {value;region=_} ->
    self init value.inside
  | TModA {value;region=_} ->
    self init value.field
  | TArg    _
  | TVar    _
  | TInt    _
  | TString _ -> init

let rec fold_expression : 'a folder -> 'a -> expr -> 'a = fun f init e  ->
  let self = fold_expression f in
  let self_type = fold_type_expression f in
  let self_module = fold_module f in
  let init = f.e init e in
  let bin_op value =
    let {op=_;arg1;arg2} = value in
    let res = self init arg1 in
    let res = self res  arg2 in
    res
  in
  match e with
    ECase    {value;region=_} ->
    let {kwd_match=_;expr;kwd_with=_;lead_vbar=_;cases} = value in
    let res = self init expr in
    let res = matching_cases self res cases in
    res
  | ECond    {value;region=_} ->
    let {kwd_if=_;test;kwd_then=_;ifso;ifnot} = value in
    let res = self init test in
    let res = self res ifso in
    (match ifnot with
    | None -> res
    | Some (_,e) -> self res e
    )
  | EAnnot   {value;region=_} ->
    let (expr, _, type_expr) = value.inside in
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
  | EArith Mod   {value;region=_} 
  | EArith Land  {value;region=_}
  | EArith Lor   {value;region=_}
  | EArith Lxor  {value;region=_}
  | EArith Lsl   {value;region=_} 
  | EArith Lsr   {value;region=_} ->
    bin_op value
  | EArith Neg   {value;region=_} ->
    let {op=_;arg} = value in
    let res = fold_expression f init arg in
    res
  | EArith Int   _
  | EArith Nat   _
  | EArith Mutez _ -> init
  | EString Cat {value;region=_} -> bin_op value
  | EString String   _
  | EString Verbatim _ -> init
  | EList ECons {value;region=_} -> bin_op value
  | EList EListComp {value;region=_} ->
    List.fold ~f:self ~init @@ pseq_to_list value.elements
  | EConstr {value;region=_} ->
    let _, expr = value in
    (match expr with
      None -> init
    | Some e -> self init e
    )
  | ERecord  {value;region=_} ->
    let aux init ({value;region=_} : _ reg) =
      let {field_name=_;assignment=_;field_expr} = value in
      let res = self init field_expr in
      res
    in
    List.Ne.fold_left aux init @@ npseq_to_ne_list value.ne_elements
  | EProj    _ -> init
  | EUpdate  {value;region=_} ->
    let aux init ({value;region=_} : _ reg) =
      let {field_path=_;assignment=_;field_expr} = value in
      let res = self init field_expr in
      res
    in
    List.Ne.fold_left aux init @@ npseq_to_ne_list value.updates.value.ne_elements
  | EModA    {value;region=_} -> self init value.field
  | EVar     _ -> init
  | ECall    {value;region=_} ->
    let (lam, args) = value in
    let res = self init lam in
    List.Ne.fold_left self res @@ args
  | EBytes   _ -> init
  | EUnit    _ -> init
  | ETuple   {value;region=_} ->
    List.Ne.fold_left self init @@ npseq_to_ne_list value
  | EPar     {value;region=_} ->
    self init value.inside
  | ELetIn   {value;region=_} ->
    let {kwd_let=_;kwd_rec=_;binding;kwd_in=_;body;attributes=_} = value in
    let {binders=_;lhs_type;eq=_;let_rhs} = binding in
    let res = self init let_rhs in
    let res = self res body in
    (match lhs_type with
      Some (_, ty) -> self_type res ty
    | None ->    res
    )
  | ETypeIn  {value;region=_} ->
    let {type_decl;kwd_in=_;body} = value in
    let {kwd_type=_;name=_;eq=_;type_expr} = type_decl in
    let res = self_type init type_expr in
    let res = self res body in
    res
  | EModIn  {value;region=_} ->
    let {mod_decl;kwd_in=_;body} = value in
    let {kwd_module=_;name=_;eq=_;kwd_struct=_;module_;kwd_end=_} = mod_decl in
    let res = self_module init module_ in
    let res = self res body in
    res
  | EModAlias {value;region=_} ->
    let {mod_alias;kwd_in=_;body} = value in
    let {kwd_module=_;alias=_;eq=_;binders=_} = mod_alias in
    let res = self init body in
    res
  | EFun     {value;region=_} ->
    let {kwd_fun=_; binders=_; lhs_type; arrow=_; body} = value in
    let res = self init body in
    (match lhs_type with
      Some (_, ty) -> self_type res ty
    | None ->    res
    )
  | ESeq     {value;region=_} ->
    List.fold ~f:self ~init @@ pseq_to_list value.elements
  | ECodeInj {value;region=_} ->
    let {language=_;code;rbracket=_} = value in
    self init code

and matching_cases self init ({value;region=_}: _ reg) =
  List.Ne.fold_left(case_clause self) init @@ npseq_to_ne_list value

and case_clause self init ({value;region=_}: _ case_clause reg) =
  let {pattern=_;arrow=_;rhs} = value in
  self init rhs

and fold_declaration : 'a folder -> 'a -> declaration -> 'a =
  fun f init d ->
  let self_expr = fold_expression f in
  let self_type = fold_type_expression f in
  let self_module = fold_module f in
  let init = f.d init d in
  match d with
    Let {value;region=_} ->
    let (_,_,let_binding,_) = value in
    let {binders=_;lhs_type;eq=_;let_rhs} = let_binding in
    let res = self_expr init let_rhs in
    (match lhs_type with
      Some (_, ty) -> self_type res ty
    | None ->    res
    )
  | TypeDecl {value;region=_} ->
    let {kwd_type=_;name=_;eq=_;type_expr} = value in
    let res = self_type init type_expr in
    res

  | ModuleDecl {value;region=_} ->
    let {kwd_module=_;name=_;eq=_;kwd_struct=_;module_;kwd_end=_} = value in
    let res = self_module init module_ in
    res
  | ModuleAlias {value;region=_} ->
    let {kwd_module=_;alias=_;eq=_;binders=_} = value in
    init
  | Directive _ -> init

and fold_module : 'a folder -> 'a -> t -> 'a =
  fun f init {decl;eof=_} ->
  let self = fold_declaration f in
  List.Ne.fold_left self init @@ decl

type ('err) mapper = {
  e : expr -> expr;
  t : type_expr -> type_expr ;
  d : declaration -> declaration ;
}

let rec map_type_expression : ('err) mapper -> type_expr -> 'b = fun f t ->
  let self = map_type_expression f in
  let t = f.t t in
  let return a = a in
  match t with
    TProd   {value;region} ->
    let value = map_npseq self value in
    return @@ TProd {value;region}
  | TSum    {value;region} ->
    let aux (e : variant reg) =
      let arg = Option.map ~f:(fun (a,b) -> let b = self b in (a,b)) e.value.arg in
      let value = {e.value with arg} in
      {e with value}
    in
    let variants = map_npseq aux value.variants in
    let value = {value with variants} in
    return @@ TSum {value;region}
  | TRecord {value;region} ->
    let aux (element : _ reg ) =
      let field_type = self element.value.field_type in
      let value = {element.value with field_type} in
      {element with value }
    in
    let ne_elements = map_npseq aux value.ne_elements in
    let value = {value with ne_elements} in
    return @@ TRecord {value;region}
  | TApp x ->
    let (constr, args) = x.value in
    let args = match args with
      | CArgTuple x ->
        let inside = map_npseq self x.value.inside in
        let x = { x with value = {x.value with inside}} in
        CArgTuple x
      | CArg x ->
        let x = self x in
        CArg x
    in
    TApp {x with value = (constr, args)}
  | TFun    {value;region} ->
    let (ty1, wild, ty2) = value in
    let ty1 = self ty1 in
    let ty2 = self ty2 in
    let value = (ty1, wild, ty2) in
    return @@ TFun {value;region}
  | TPar    {value;region} ->
    let inside = self value.inside in
    let value = {value with inside} in
    return @@ TPar {value;region}
  | TModA {value;region} ->
    let field = self value.field in
    let value = {value with field} in
    return @@ TModA {value;region}
  | TArg _
  | TVar    _
  | TInt _
  | TString _ -> t

let rec map_expression : ('err) mapper -> expr -> expr = fun f e  ->
  let self = map_expression f in
  let self_type = map_type_expression f in
  let self_module = map_module f in
  let return a = a in
  let e = f.e e in
  let bin_op value =
    let {op;arg1;arg2} = value in
    let arg1 = self arg1 in
    let arg2 = self arg2 in
    {op;arg1;arg2}
  in
  match e with
    ECase    {value;region} ->
    let {kwd_match=_;expr;kwd_with=_;lead_vbar=_;cases} = value in
    let expr = self expr in
    let cases = matching_cases self cases in
    let value = {value with expr;cases} in
    return @@ ECase {value;region}
  | ECond    {value;region} ->
    let {kwd_if;test;kwd_then;ifso;ifnot} = value in
    let test = self test in
    let ifso = self ifso in
    let ifnot = Option.map ~f:(fun (a,b) ->
      let b = self b in (a,b)) ifnot in
    let value = {kwd_if;test;kwd_then;ifso;ifnot} in
    return @@ ECond {value;region}
  | EAnnot   {value;region} ->
    let expr, comma, type_expr = value.inside in
    let expr = self expr in
    let type_expr = self_type type_expr in
    let inside = expr, comma, type_expr in
    let value = {value with inside} in
    return @@ EAnnot {value;region}
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
  | EArith Land   {value;region} ->
    let value = bin_op value in
    return @@ EArith (Land {value;region})
  | EArith Lor   {value;region} ->
    let value = bin_op value in
    return @@ EArith (Lor {value;region})
  | EArith Lxor   {value;region} ->
    let value = bin_op value in
    return @@ EArith (Lxor {value;region})
  | EArith Lsl   {value;region} ->
    let value = bin_op value in
    return @@ EArith (Lsl {value;region})
  | EArith Lsr   {value;region} ->
    let value = bin_op value in
    return @@ EArith (Lsr {value;region})
  | EArith Neg   {value;region} ->
    let arg = self value.arg in
    let value = {value with arg} in
    return @@ EArith (Neg {value;region})
  | EArith Int   _
  | EArith Nat   _
  | EArith Mutez _ as e -> return @@ e
  | EString Cat {value;region} ->
    let value = bin_op value in
    return @@ EString (Cat {value;region})
  | EString String   _
  | EString Verbatim _ as e -> return @@ e
  | EList ECons {value;region} ->
    let value = bin_op value in
    return @@ EList (ECons {value;region})
  | EList EListComp {value;region} ->
    let elements = map_pseq self value.elements in
    let value = {value with elements} in
    return @@ EList (EListComp {value;region})
  | EConstr {value;region} ->
    let const, expr = value in
    let expr = Option.map ~f:self expr in
    let value = const,expr in
    return @@ EConstr {value;region}
  | ERecord  {value;region} ->
    let aux (e : field_assign reg) =
      let field_expr = self e.value.field_expr in
      {e with value = {e.value with field_expr}}
    in
    let ne_elements = map_npseq aux value.ne_elements in
    let value = {value with ne_elements} in
    return @@ ERecord {value;region}
  | EProj    _  as e -> return @@ e
  | EUpdate  {value;region} ->
    let aux (e : field_path_assignment reg) =
      let field_expr = self e.value.field_expr in
      {e with value = {e.value with field_expr}}
    in
    let ne_elements = map_npseq aux value.updates.value.ne_elements in
    let updates = {value.updates with value = {value.updates.value with ne_elements}} in
    let value = {value with updates} in
    return @@ EUpdate {value;region}
  | EModA {value;region} ->
    let field = self value.field in
    let value = {value with field} in
    return @@ EModA {value;region}
  | EVar     _ as e -> return e
  | ECall    {value;region} ->
    let (lam, args) = value in
    let lam = self lam in
    let args = List.Ne.map self @@ args in
    let value = (lam,args) in
    return @@ ECall {value;region}
  | EBytes   _ as e -> return @@ e
  | EUnit    _ as e -> return @@ e
  | ETuple   {value;region} ->
    let value = map_npseq self value in
    return @@ ETuple {value;region}
  | EPar     {value;region} ->
    let inside = self value.inside in
    let value = {value with inside} in
    return @@ EPar {value;region}
  | ELetIn   {value;region} ->
    let {kwd_let=_;kwd_rec=_;binding;kwd_in=_;body;attributes=_} = value in
    let {binders;type_params;lhs_type;eq;let_rhs} = binding in
    let let_rhs = self let_rhs in
    let lhs_type = Option.map ~f:(fun (a,b) -> (a,self_type b)) lhs_type in
    let binding : let_binding = {binders;type_params;lhs_type;eq;let_rhs} in
    let body = self body in
    let value = {value with binding;body} in
    return @@ ELetIn {value;region}
  | ETypeIn  {value;region} ->
    let {type_decl;kwd_in;body} = value in
    let {kwd_type=_;name=_;eq=_;type_expr} = type_decl in
    let type_expr = self_type type_expr in
    let body = self body in
    let type_decl = {type_decl with type_expr} in
    let value = {type_decl;kwd_in;body} in
    return @@ ETypeIn {value;region}
  | EModIn  {value;region} ->
    let {mod_decl;kwd_in;body} = value in
    let {kwd_module=_;name=_;eq=_;kwd_struct=_;module_;kwd_end=_} = mod_decl in
    let module_ = self_module module_ in
    let body = self body in
    let mod_decl = {mod_decl with module_} in
    let value = {mod_decl;kwd_in;body} in
    return @@ EModIn {value;region}
  | EModAlias  {value;region} ->
    let {mod_alias;kwd_in;body} = value in
    let {kwd_module=_;alias=_;eq=_;binders=_} = mod_alias in
    let body = self body in
    let value = {mod_alias;kwd_in;body} in
    return @@ EModAlias {value;region}
  | EFun     {value;region} ->
    let {kwd_fun=_; binders=_; lhs_type; arrow=_; body} = value in
    let body = self body in
    let lhs_type = Option.map ~f:(fun (a,b) ->
      let b = self_type b in (a,b)) lhs_type in
    let value = {value with body;lhs_type} in
    return @@ EFun {value;region}
  | ESeq     {value;region} ->
    let elements = map_pseq self value.elements in
    let value = {value with elements} in
    return @@ ESeq {value;region}
  | ECodeInj {value;region} ->
    let code = self value.code in
    let value = {value with code} in
    return @@ ECodeInj {value;region}

and matching_cases self (cases: _ Utils.nsepseq reg) =
  let value = map_npseq (case_clause self) @@ cases.value in
  {cases with value}

and case_clause self (case_clause: _ case_clause reg) =
  let {pattern=_;arrow=_;rhs} = case_clause.value in
  let rhs = self rhs in
  let value = {case_clause.value with rhs} in
  {case_clause with value}

and map_declaration : ('err) mapper -> declaration -> declaration =
  fun f d ->
  let self_expr = map_expression f in
  let self_type = map_type_expression f in
  let self_module = map_module f in
  let return a = a in
  let d = f.d d in
  match d with
    Let {value;region} ->
    let (kwd_let,kwd_rec,let_binding,attr) = value in
    let {binders;type_params;lhs_type;eq;let_rhs} = let_binding in
    let let_rhs = self_expr let_rhs in
    let lhs_type =  Option.map ~f:(fun (a,b) -> (a,self_type b)) lhs_type in
    let let_binding = {binders;type_params;lhs_type;eq;let_rhs} in
    let value = (kwd_let,kwd_rec,let_binding,attr) in
    return @@ Let {value;region}
  | TypeDecl {value;region} ->
    let {kwd_type=_;name=_;eq=_;type_expr} = value in
    let type_expr = self_type type_expr in
    let value = {value with type_expr} in
    return @@ TypeDecl {value;region}
  | ModuleDecl {value;region} ->
    let {kwd_module=_;name=_;eq=_;kwd_struct=_;module_;kwd_end=_} = value in
    let module_ = self_module module_ in
    let value = {value with module_} in
    return @@ ModuleDecl {value;region}
  | ModuleAlias {value;region} ->
    let {kwd_module=_;alias=_;eq=_;binders=_} = value in
    return @@ ModuleAlias {value;region}
  | Directive _ as d -> return d

and map_module : ('err) mapper -> t -> t =
  fun f {decl;eof} ->
  let self = map_declaration f in
  (fun decl -> {decl;eof}) @@
  List.Ne.map self @@ decl

(* TODO this is stupid *)
let fold_to_map : unit -> unit folder -> 'err mapper =
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
