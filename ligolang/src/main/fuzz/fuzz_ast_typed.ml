open Ast_typed
include Fuzz_shared.Monad

type mutation = Location.t * expression

let get_mutation_id (loc, expr) =
  let s = Format.asprintf  "%a%a" Location.pp loc Ast_typed.PP.expression expr in
  let hash = Tezos_crypto.Base58.raw_encode @@ Bytes.to_string @@ Tezos_crypto.Hacl.Hash.Keccak_256.digest (Bytes.of_string s) in
  String.sub hash 0 8

let add_line : Buffer.t -> string -> unit = fun buffer s ->
  Buffer.add_string buffer (Format.asprintf "%s\n" s)

let consume_lines : in_channel -> int -> string =
  fun in_chan stop ->
  let rec loop_lines curr line =
    if curr >= stop then
      line
    else
      let curr = curr + 1 in
      let line = input_line in_chan in
      loop_lines curr line in
  loop_lines 0 ""

let add_lines_to_buffer : in_channel -> int -> Buffer.t -> unit =
  fun in_chan stop buffer ->
  let rec loop_lines curr =
    if curr >= stop then
      ()
    else
      let curr = curr + 1 in
      let line = input_line in_chan in
      let () = add_line buffer line in
      loop_lines curr in
  loop_lines 0

let add_all_lines_to_buffer : in_channel -> Buffer.t -> unit =
  fun in_chan buffer ->
  let rec loop_lines () =
    try (let line = input_line in_chan in
         let () = add_line buffer line in
         loop_lines ()) with
    | _ -> () in
  loop_lines ()

let buffer_of_mutation : mutation -> Buffer.t = fun (loc, _expr) ->
  match Location.get_file loc with
  | Some r ->
     (* Open file *)
     let file = r # file in
     let ic = open_in_bin file in
     (* Decompile expression *)
     let n_syntax     = Trace.to_stdlib_result (Decompile.Helpers.syntax_to_variant (Syntax_name "auto") (Some file)) in
     let n_syntax     = match n_syntax with
       | Ok r -> r
       | Error _ -> failwith "Cannot detect syntax" in
     let core         = Decompile.Of_typed.decompile_expression _expr in
     let sugar        = Decompile.Of_core.decompile_expression core in
     let imperative   = Decompile.Of_sugar.decompile_expression sugar in
     let buffer       = Decompile.Of_imperative.decompile_expression imperative n_syntax in
     (* Create buffer *)
     let output_buffer = Buffer.create 0 in
     (* Read first lines *)
     let () = add_lines_to_buffer ic (r # start # line - 1) output_buffer in
     let s = if (r # start # line = r # stop # line) then
               let line = input_line ic in
               let prefix = String.sub line 0 (r # start # offset `Byte) in
               let postfix = String.sub line (r # stop # offset `Byte) (String.length line - r # stop # offset `Byte) in
               prefix ^ Buffer.contents buffer ^ postfix
             else
               let line = input_line ic in
               let prefix = String.sub line 0 (r # start # offset `Byte) in
               let line = consume_lines ic (r # stop # line - r # start # line) in
               let postfix = String.sub line (r # stop # offset `Byte) (String.length line - r # stop # offset `Byte) in
               prefix ^ Buffer.contents buffer ^ postfix in
     let () = add_line output_buffer s in
     (* Read rest of lines *)
     let () = add_all_lines_to_buffer ic output_buffer in
     output_buffer
  | None ->
     failwith "Error when constructing buffer from mutation: file location was expected"

(* Helpers for changing operators *)
let map_constant cons_name arguments final_type_expression =
  let possible_const =
    match arguments with
      [ e1 ] ->
       let t1 = e1.type_expression in
       let t2 = final_type_expression in
       if is_t_nat t1 && is_t_int t2 then
         [C_NOT;C_NEG]
       else if is_t_int t1 && is_t_int t2 then
         [C_NOT;C_NEG]
       else
         []
    | [ e1 ; e2 ] ->
       let t1 = e1.type_expression in
       let t2 = e2.type_expression in
       let t3 = final_type_expression in
       let compare = [C_EQ; C_NEQ; C_LT; C_GT; C_LE; C_GE] in
       if is_t_nat t1 && is_t_nat t2 && is_t_nat t3 then
         [C_ADD;C_MUL;C_DIV;C_OR; C_AND; C_XOR]
       else if is_t_int t1 && is_t_int t2 && is_t_int t3 then
         [C_ADD;C_MUL;C_DIV;C_SUB]
       else if is_t_bool t1 && is_t_bool t2 && is_t_bool t3 then
         [C_OR; C_AND; C_XOR]
       else if is_t_mutez t1 && is_t_mutez t2 && is_t_mutez t3 then
         [C_ADD;C_SUB]

       else if is_t_int t1 && is_t_nat t2 && is_t_int t3 then
         [C_ADD;C_MUL;C_SUB]
       else if is_t_nat t1 && is_t_int t2 && is_t_int t3 then
         [C_ADD;C_MUL;C_SUB]

       else if type_expression_eq (t1, t2) && is_t_bool t3 then
         compare

       else
         []
     | _ ->
       [] in
  if List.mem possible_const cons_name ~equal:(fun c1 c2 -> Ast_typed.Compare.constant' c1 c2 = 0) then
    possible_const
  else
     [cons_name]

(* Helpers for transforming literals *)

let transform_int =
  let const0 _ = 0 in
  let negative n = -n in
  let incr n = n + 1 in
  let pred n = n - 1 in
  let prod n = 2 * n in
  [const0; negative; incr; pred; prod]

let transform_nat =
  let const0 _ = 0 in
  let incr n = n + 1 in
  let prod n = 2 * n in
  [const0; incr; prod]

let transform_string =
  let constn _ = "" in
  let double s = s ^ s in
  [String.capitalize_ascii; String.uncapitalize_ascii; String.lowercase_ascii; String.uppercase_ascii; constn; double]

module Mutator = struct
  let combine : 'a -> ('a * mutation option) list -> 'b -> ('b * mutation option) list -> ('a * 'b * mutation option) list =
    fun a al b bl ->
    List.map ~f:(fun (b, m) -> (a, b, m)) bl @ List.map ~f:(fun (a, m) -> (a, b, m)) al

  let combine_list : 'a list -> (('a * mutation option) list) list -> ('a list * mutation option) list =
    fun a al ->
    List.concat @@ List.mapi ~f:(fun i ali ->
      List.map ~f:(fun (v, m) -> (List.take a i @ [ v ] @ List.drop a (i + 1),  m)) ali) al

  let (let+) x f = List.map ~f x
  let (let*) x f = List.concat (List.map ~f x)
  let  return x = [x]

  let mutate_literal = function
    | Literal_int z ->
       let z = Z.to_int z in
       let* t = transform_int in
       let m = t z in
       return (Literal_int (Z.of_int m), z <> m)
    | Literal_nat z ->
       let n = Z.to_int z in
       let* t = transform_nat in
       let m = t n in
       return (Literal_nat (Z.of_int m), n <> m)
    | Literal_mutez z ->
       let n = Z.to_int z in
       let* t = transform_nat in
       let m = t n in
       return (Literal_mutez (Z.of_int m), n <> m)
    | Literal_string (Standard s) ->
       let* t = transform_string in
       let m = t s in
       return (Literal_string (Ligo_string.standard m), not (String.equal s m))
    | l ->
       return (l, false)

  let mutate_constant ({cons_name; arguments} as const) final_type =
    let ops = List.remove_element ~compare:(fun c1 c2 -> Ast_typed.Compare.constant' c1 c2) cons_name @@
                map_constant cons_name arguments final_type in
    let mapper x =
        ({ const with cons_name = x }), true in
    let swapper cons_name arguments =
      match cons_name with
      | C_CONCAT ->
         [({cons_name;arguments=List.rev arguments}, true)]
      | _ ->
         [] in
    [(const, false)] @ List.map ~f:mapper ops @ swapper cons_name arguments

  let rec mutate_expression : expression -> (expression * mutation option) list = fun e' ->
    let return expression_content = { e' with expression_content } in
    let self = mutate_expression in
    match e'.expression_content with
    | E_matching {matchee;cases} -> (
      let+ matchee, cases, mutation = combine matchee (self matchee) cases (mutate_cases cases) in
      return @@ E_matching {matchee;cases=cases}, mutation
    )
    | E_record_accessor {record; path} -> (
      let+ record, mutation = self record in
      return @@ E_record_accessor {record; path}, mutation
    )
    | E_record m -> (
      let ml = LMap.to_kv_list m in
      let mls = List.map ~f:(fun (l, v) -> let* h,m = self v in [((l, h), m)]) ml in
      let+ m', mutation = combine_list ml mls in
      return @@ E_record (LMap.of_list m'), mutation
    )
    | E_record_update {record; path; update} -> (
      let+ record, update, mutation = combine record (self record) update (self update) in
      return @@ E_record_update {record;path;update}, mutation
    )
    | E_constructor c -> (
      let+ e', mutation = self c.element in
      return @@ E_constructor {c with element = e'}, mutation
    )
    | E_application {lamb; args} -> (
      let+ a, b, mutation = combine lamb (self lamb) args (self args) in
      return @@ E_application {lamb=a;args=b}, mutation
    )
    | E_let_in { let_binder ; rhs ; let_result; attr } -> (
      if attr.no_mutation then
        let+ let_result, mutation = self let_result in
        return @@ E_let_in { let_binder ; rhs ; let_result; attr }, mutation
      else
        let+ rhs, let_result, mutation = combine rhs (self rhs) let_result (self let_result) in
        return @@ E_let_in { let_binder ; rhs ; let_result; attr }, mutation
    )
    | E_type_in {type_binder; rhs; let_result} -> (
      let+ let_result, mutation = self let_result in
      return @@ E_type_in {type_binder;rhs;let_result}, mutation
    )
    | E_mod_in { module_binder ; rhs ; let_result } -> (
      let+ rhs, let_result, mutation = combine rhs (mutate_module rhs) let_result (self let_result) in
      return @@ E_mod_in { module_binder ; rhs ; let_result }, mutation
    )
    | E_mod_alias { alias ; binders ; result } -> (
      let+ result, mutation = self result in
      return @@ E_mod_alias { alias ; binders ; result }, mutation
    )
    | E_lambda { binder ; result } -> (
      let+ result, mutation = self result in
      return @@ E_lambda { binder ; result }, mutation
    )
    | E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
      let+ result, mutation = self result in
      return @@ E_recursive { fun_name; fun_type; lambda = {binder;result}}, mutation
    )
    | E_constant c -> (
      let cb = mutate_constant c e'.type_expression in
      let cb = List.map ~f:(fun (cc, b) -> (cc.cons_name, Option.some_if (b && not (Location.is_dummy_or_generated e'.location)) (e'.location, return @@ E_constant cc))) cb in
      let argumentsm = combine_list c.arguments (List.map ~f:self c.arguments) in
      let+ a,b,mutation = combine c.cons_name cb c.arguments argumentsm in
      return @@ E_constant {cons_name= a; arguments = b}, mutation
    )
    | E_module_accessor { module_name; element } -> (
      let+ element, mutation = self element in
      return @@ E_module_accessor { module_name; element }, mutation
    )
    | E_literal l -> (
      let+ l, b = mutate_literal l in
      let e = return @@ E_literal l in
      e, Option.some_if (b && not (Location.is_dummy_or_generated e.location)) (e.location, e)
    )
    | E_variable _ | E_raw_code _ as e' -> [ (return e'), None ]
    | E_type_inst _ as e' -> [ (return e'), None ]

  and mutate_cases : matching_expr -> (matching_expr * mutation option) list = fun m ->
    match m with
    | Match_variant {cases;tv} -> (
      let aux { constructor ; pattern ; body } =
        let+ body, mutation = mutate_expression body in
        ({constructor;pattern;body}, mutation)
      in
      let casess = List.map ~f:aux cases in
      let+ cases, mutation = combine_list cases casess in
      Match_variant {cases ; tv}, mutation
    )
    | Match_record {fields; body; tv} ->
       let+ body, mutation = mutate_expression body in
       Match_record {fields; body; tv}, mutation

  and mutate_module : module_fully_typed -> (module_fully_typed * mutation option) list = fun (Module_Fully_Typed p) ->
    let aux = fun ({location; wrap_content = x} : declaration location_wrap) ->
      match x with
      | Declaration_constant {name; binder; expr ; attr} -> (
        if attr.no_mutation then
          [(({location; wrap_content = Declaration_constant {name; binder; expr ; attr}} : declaration location_wrap), None)]
        else
          let+ expr, mutation = mutate_expression expr in
          ({location; wrap_content = Declaration_constant {name; binder; expr ; attr}} : declaration location_wrap), mutation
      )
      | Declaration_type t -> [ ({location; wrap_content = Declaration_type t} : declaration location_wrap), None ]
      | Declaration_module {module_binder;module_; module_attr} ->
         let+ module_, mutation = mutate_module module_ in
         ({location; wrap_content = Declaration_module {module_binder; module_; module_attr}} : declaration location_wrap), mutation
      | Module_alias _ -> [ ({location; wrap_content = x} : declaration location_wrap), None ]
    in
    let* p, mutation = combine_list p (List.map ~f:aux p) in
    [ Module_Fully_Typed p, mutation ]

  let some_mutate_expression ?(n = 0) (expr : Ast_typed.expression) =
    List.nth (List.filter_map ~f:(fun (v, i) -> Option.map i ~f:(fun m -> (v, m))) (mutate_expression expr))
             n

  let all_mutate_expression (expr : Ast_typed.expression) =
    List.filter_map ~f:(fun (v, i) -> Option.map i ~f:(fun m -> (v, m))) (mutate_expression expr)

end
