open Simple_utils
open Trace
open Zinc_types.Raw
module AST = Ast_typed
open Ast_typed.Types

(*
  Optimizations to do:
  1. Calls to known functions should be optimized. No need to 
     check at runtime whether they've been given enough arguments 
     because we can easily check that at compile time instead.
  2. We convert top-level lets to let expressions in the definitions 
     that come after them. We should only do this if the value is a 
     transitive dependency.
*)

(* Types defined in ../../stages/6deku-zinc/types.ml *)

type environment = { binders : AST.expression_ Var.t list }

let empty_environment = { binders = [] }

(*** Adds a binder to the environment.
     For example, in `let a=b in c`, `a` is a binder and it needs to be added to the environment when compiling `c` *)
let add_binder x = function { binders } -> { binders = x :: binders }

(*** Adds a declaration name to the environment.
     For example, in `let a=b; let c=d;`, `a` is a declaration name and it needs to be added to the environment when compiling `d` *)

(*** Compiles a type from the ast_typed representation to mini-c's, which is substantially simpler and more useful for us *)
let compile_type ~(raise : Errors.zincing_error raise) t =
  t |> Spilling.compile_type ~raise |> fun x -> x.type_content

let rec tail_compile :
    raise:Errors.zincing_error raise -> environment -> AST.expression -> Zinc.t
    =
 (*** For optimization purposes, we have one function for compiling expressions in the "tail position" and another for
     compiling everything else. *)
 fun ~raise environment expr ->
  let () =
    print_endline
      (Format.asprintf "tail compile: %a / env: %s" AST.PP.expression expr
         (environment.binders
         |> List.map ~f:(Format.asprintf "%a" Var.pp)
         |> String.concat ","))
  in
  let tail_compile = tail_compile ~raise in
  let other_compile = other_compile ~raise in
  let compile_let environment ~let':name ~equal:value ~in':expression =
    let result_compiled =
      tail_compile (environment |> add_binder name) expression
    in
    other_compile environment ~k:(Core Grab :: result_compiled) value
  in
  let compile_known_function_application =
    compile_known_function_application ~raise
  in
  let compile_function_application ~function_compiler environment expr args =
    compile_known_function_application environment
      (function_compiler environment expr)
      args
  in
  let compile_pattern_matching =
    compile_pattern_matching ~raise ~compile_match_code:tail_compile
  in

  match expr.expression_content with
  | E_lambda lambda ->
      Core Grab
      :: tail_compile
           (environment |> add_binder lambda.binder.wrap_content)
           lambda.result
  | E_let_in { let_binder; rhs; let_result; _ } ->
      compile_let environment ~let':let_binder.wrap_content ~equal:rhs
        ~in':let_result
  (* TODO: function applications are disagregated in typed_ast, this defeats the whole purpose of using zinc, need to fix this *)
  | E_application { lamb; args } ->
      compile_function_application ~function_compiler:tail_compile environment
        lamb [ args ]
  | E_matching { matchee; cases = Match_record match_record } ->
      tail_compile environment (match_record_rewrite ~matchee match_record)
  | E_matching { matchee; cases = Match_variant match_variant } ->
      compile_pattern_matching environment ~matchee match_variant ~k:[]
  | _ -> other_compile environment ~k:[ Core Return ] expr

(*** For optimization purposes, we have one function for compiling expressions in the "tail position" and another for
     compiling everything else. *)
and other_compile :
    raise:Errors.zincing_error raise ->
    environment ->
    AST.expression ->
    k:Zinc.t ->
    Zinc.t =
 fun ~raise environment expr ~k ->
  let () =
    print_endline
      (Format.asprintf "other compile: %a / ~k:%s / env: %s" AST.PP.expression
         expr (Zinc.to_string k)
         (environment.binders
         |> List.map ~f:(Format.asprintf "%a" Var.pp)
         |> String.concat ","))
  in
  let tail_compile = tail_compile ~raise in
  let other_compile = other_compile ~raise in
  let compile_pattern_matching =
    compile_pattern_matching ~raise
      ~compile_match_code:(other_compile ~k:[ Core EndLet ])
  in
  let compile_let environment ~let':name ~equal:value ~in':expression =
    let result_compiled =
      other_compile
        (environment |> add_binder name)
        expression ~k:(Core EndLet :: k)
    in
    other_compile environment ~k:(Core Grab :: result_compiled) value
  in
  let compile_known_function_application environment compiled_func args ~k =
    compile_known_function_application ~raise environment (compiled_func ~k)
      args
  in
  let compile_function_application ~function_compiler environment expr args ~k =
    Zinc.Core (PushRetAddr k)
    :: compile_known_function_application environment
         (fun ~k ->
           function_compiler environment expr ~k:(Zinc.Core Apply :: k))
         args ~k
  in
  match expr.expression_content with
  | E_literal literal -> (
      match literal with
      | Literal_int x -> Plain_old_data (Num x) :: k
      | Literal_address s -> Plain_old_data (Address s) :: k
      | Literal_bytes b -> Plain_old_data (Bytes b) :: k
      | Literal_string (Standard b) -> Plain_old_data (String b) :: k
      | Literal_string (Verbatim b) -> Plain_old_data (String b) :: k
      | Literal_mutez a -> Plain_old_data (Mutez a) :: k
      | Literal_key a -> Plain_old_data (Key a) :: k
      | Literal_unit -> Adt (MakeRecord 0) :: k
      | x ->
          failwith
            (Format.asprintf "literal type not supported: %a"
               Ast_typed.PP.literal x))
  | E_constant constant ->
      let compiled_constant =
        compile_constant ~raise expr.type_expression constant
      in
      compile_known_function_application environment compiled_constant
        constant.arguments ~k
  | E_variable ({ wrap_content = variable; location = _ } as binder) -> (
      let find_index x lst =
        let rec func x lst c =
          match lst with
          | [] -> None
          | hd :: tl -> if hd = x then Some c else func x tl (c + 1)
        in
        func x lst 0
      in

      match find_index variable environment.binders with
      | None ->
          failwith
            (Format.asprintf "binder %a not found in environment!"
               AST.PP.expression_variable binder)
      | Some index -> Core (Access index) :: k)
  (* TODO: function applications are disagregated in typed_ast, this defeats the whole purpose of using zinc, need to fix this *)
  | E_application { lamb; args } ->
      compile_function_application ~function_compiler:other_compile environment
        lamb [ args ] ~k
  | E_lambda { binder = { wrap_content = binder; _ }; result } ->
      Core
        (Closure
           (Core Grab :: tail_compile (environment |> add_binder binder) result))
      :: k
  | E_recursive _recursive -> failwith "E_recursive unimplemented"
  | E_let_in { let_binder; rhs; let_result; _ } ->
      compile_let environment ~let':let_binder.wrap_content ~equal:rhs
        ~in':let_result
  | E_type_in _type_in -> failwith "E_type_in unimplemented"
  | E_mod_in _mod_in -> failwith "E_mod_in unimplemented"
  | E_mod_alias _mod_alias -> failwith "E_mod_alias unimplemented"
  | E_raw_code _raw_code -> failwith "E_raw_code unimplemented" (* Variant *)
  | E_constructor { constructor = Label name; element }
    when String.equal name "True" && element.expression_content = AST.e_unit ()
    ->
      compile_constant ~raise expr.type_expression
        { cons_name = C_TRUE; arguments = [] }
        ~k
  | E_constructor { constructor = Label name; element }
    when String.equal name "False" && element.expression_content = AST.e_unit ()
    ->
      compile_constant ~raise expr.type_expression
        { cons_name = C_FALSE; arguments = [] }
        ~k
  | E_constructor { constructor = Label constructor; element } ->
      let idx =
        expr.type_expression.type_content |> function
        | T_sum r | T_record r ->
            let rows = LMap.keys r.content in
            List.findi ~f:(fun _ (Label x) -> constructor = x) rows
            |> Stdlib.Option.get |> fst
        | T_constant const when Ligo_string.extract const.injection = "option"
          -> (
            match constructor with
            | "Some" -> 0
            | "None" -> 1
            | x -> failwith @@ Format.asprintf "invalid constructor %s" x)
        | x ->
            failwith
            @@ Format.asprintf "Unsupported type in pattern match: %a\n"
                 AST.PP.type_content x
      in
      compile_known_function_application environment
        (fun ~k -> Adt (MakeVariant idx) :: k)
        [ element ] ~k
  | E_matching { matchee; cases = Match_record match_record } ->
      other_compile environment (match_record_rewrite ~matchee match_record) ~k
  | E_matching { matchee; cases = Match_variant match_variant } ->
      compile_pattern_matching environment ~matchee match_variant ~k
  (* Record *)
  | E_record expression_label_map ->
      let bindings = Stage_common.Types.LMap.bindings expression_label_map in
      compile_known_function_application environment
        (fun ~k -> Adt (MakeRecord (List.length bindings)) :: k)
        (List.map ~f:(fun (_, value) -> value) bindings)
        ~k
  | E_record_accessor { record; path } ->
      let rows = record.type_expression.type_content in
      let label =
        match rows with
        | T_record rows ->
            let[@warning "-8"] (Some (label, _)) =
              let bindings = LMap.bindings rows.content in
              List.findi bindings ~f:(fun _ (k, _) -> k = path)
            in
            label
        | T_constant _typ ->
            let (Stage_common.Types.Label path) = path in
            int_of_string path
        | T_sum _rows ->
            let (Label path) = path in
            int_of_string path
        | x ->
            Format.asprintf "constructor %a\n" AST.PP.type_content x |> failwith
      in
      compile_known_function_application environment
        (fun ~k -> Adt (RecordAccess label) :: k)
        [ record ] ~k
  | E_record_update _record_update -> failwith "E_record_update unimplemented"
  | E_type_inst _ -> failwith "E_type_inst unimplemented"
  | E_module_accessor _module_access ->
      failwith "E_module_accessor unimplemented"

and compile_constant :
    raise:Errors.zincing_error raise ->
    AST.type_expression ->
    AST.constant ->
    k:Zinc.t ->
    Zinc.t =
 fun ~raise:_ _ constant ~k ->
  match constant.cons_name with
  | C_CHAIN_ID -> Domain_specific_operation ChainID :: k
  | C_HASH_KEY -> Operation HashKey :: k
  | C_EQ -> Operation Eq :: k
  | C_ADD -> Operation Add :: k
  | C_FAILWITH -> Control_flow Failwith :: k
  | C_CONTRACT_OPT -> Domain_specific_operation Contract_opt :: k
  | C_CALL -> Domain_specific_operation MakeTransaction :: k
  | C_UNIT -> Adt (MakeRecord 0) :: k
  | C_NONE -> Adt (MakeRecord 0) :: Adt (MakeVariant 1) :: k
  | C_SOME -> Adt (MakeVariant 0) :: k
  | C_CONS -> Operation Cons :: k
  | C_LIST_EMPTY -> Plain_old_data Nil :: k
  | C_TRUE -> Plain_old_data (Bool true) :: k
  | C_FALSE -> Plain_old_data (Bool false) :: k
  | C_OR -> Operation Or :: k
  | C_AND -> Operation And :: k
  | C_NOT -> Operation Not :: k
  | name ->
      failwith
        (Format.asprintf "Unsupported constant: %a" AST.PP.constant' name)

(*** This is for "known function"s, which is what we call functions whose definition is known at compile time. Compare to
     functions such as `f` in `List.map` - inside the function `List.map`, `f` could be anything. The important difference
     is that we know how many arguments known functions take before doing any actual work. This function assumes that you've
     ensured that you've passed exactly that many arguments, and if you don't the generated code will be wrong. Since you can
     only ensure that for known functions, it's called `compile_known_function_application`. *)
and compile_known_function_application :
    raise:Errors.zincing_error raise ->
    environment ->
    Zinc.t ->
    AST.expression list ->
    Zinc.t =
 fun ~raise environment compiled_func args ->
  let rec comp l =
    match l with
    | [] -> compiled_func
    | arg :: args -> other_compile ~raise environment ~k:(comp args) arg
  in
  args |> List.rev |> comp

(*** used to convert `[("a", expr1)] result` to `let a = expr1 in result` *)
and make_expression_with_dependencies :
    (expression_variable * AST.expression) list ->
    AST.expression ->
    AST.expression =
 fun dependencies expression ->
  let loc = Simple_utils.Location.Virtual "generated let" in
  dependencies
  |> List.fold ~init:expression ~f:(fun result (binder, expression) ->
         {
           expression_content =
             E_let_in
               {
                 let_binder = binder;
                 rhs = expression;
                 let_result = result;
                 attr =
                   {
                     inline = false;
                     no_mutation = false;
                     view = false;
                     public = false;
                   };
               };
           location = loc;
           type_expression = expression.type_expression;
         })

and match_record_rewrite :
    matchee:AST.expression -> AST.matching_content_record -> AST.expression =
 fun ~matchee -> function
  | { fields = binders; body; _ } ->
      let open Stage_common.Types in
      let fresh = Simple_utils.Var.fresh () in
      let loc =
        Simple_utils.Location.Virtual "generated let around match expression"
      in
      let dependencies =
        LMap.bindings binders
        |> List.map ~f:(fun (label, (binder, type_expression)) ->
               ( binder,
                 {
                   expression_content =
                     E_record_accessor
                       {
                         record =
                           {
                             expression_content =
                               E_variable
                                 { wrap_content = fresh; location = loc };
                             location = loc;
                             type_expression;
                           };
                         path = label;
                       };
                   location = loc;
                   type_expression;
                 } ))
      in
      let lettified = make_expression_with_dependencies dependencies body in
      let lettified =
        {
          expression_content =
            E_let_in
              {
                let_binder = { wrap_content = fresh; location = loc };
                rhs = matchee;
                let_result = lettified;
                attr =
                  {
                    inline = false;
                    no_mutation = false;
                    view = false;
                    public = false;
                  };
              };
          type_expression = matchee.type_expression;
          location = loc;
        }
      in
      lettified

and compile_pattern_matching :
    raise:Errors.zincing_error raise ->
    environment ->
    matchee:AST.expression ->
    AST.matching_content_variant ->
    compile_match_code:(environment -> AST.expression -> Zinc.t) ->
    k:Zinc.t ->
    Zinc.t =
 fun ~raise environment ~matchee cases ~compile_match_code ~k ->
  let open Zinc in
  let other_compile = other_compile ~raise in
  let compile_type = compile_type ~raise in
  let compiled_type = compile_type matchee.type_expression in
  let pattern_match_common cases fn =
    let code =
      Adt
        (MatchVariant
           (List.map
              ~f:
                (fun {
                       constructor = Label lbl;
                       pattern = { wrap_content = pattern; _ };
                       body;
                     } ->
                (* When interpreting MatchVariant, the interpreter pops the top item off the stack,
                   which better be a variant, then unwraps it, then pushes the unwraped item onto
                   the stack. We compile pattern match cases as if they were functions, so we need
                   to `Grab` the unwrapped item off the stack just like we would when compiling any
                   other function *)
                let compiled =
                  Core Grab
                  :: compile_match_code (add_binder pattern environment) body
                in
                let idx = fn lbl in
                (idx, compiled))
              cases
           |> List.sort ~compare:(fun (x, _) (y, _) -> Int.compare x y)
           |> List.map ~f:snd |> Zinc_utils.LMap.of_list))
    in
    other_compile environment matchee ~k:(code :: k)
  in
  match (compiled_type, cases) with
  | T_or _, { cases; _ } ->
      let rows =
        matchee.type_expression.type_content |> function
        | T_sum r | T_record r -> LMap.keys r.content
        | x -> failwith @@ Format.asprintf "%a\n" AST.PP.type_content x
      in
      let fn lbl =
        List.findi ~f:(fun _ (Label label) -> label = lbl) rows
        |> Stdlib.Option.get |> fst
      in
      pattern_match_common cases fn
  | T_option _, { cases; _ } ->
      pattern_match_common cases (function
        | "Some" -> 0
        | "None" -> 1
        | _ -> failwith "invalid case")
  | T_base TB_bool, { cases; _ } ->
      pattern_match_common cases (function
        | "False" -> 0
        | "True" -> 1
        | _ -> failwith "invalid case")
  | _ ->
      failwith
        (Format.asprintf
           "E_matching unimplemented. Need to implement matching for %a"
           Mini_c.PP.type_content
           (compile_type matchee.type_expression))

let compile_module :
    raise:Errors.zincing_error raise -> AST.module_fully_typed -> Program.t =
 fun ~raise modul ->
  let (Module_Fully_Typed ast) = modul in
  let constant_declaration_extractor :
      declaration_loc -> (module_variable * expression) option = function
    | { wrap_content = Declaration_constant declaration_constant; _ } ->
        let name =
          match declaration_constant.name with
          | Some name -> name
          | None -> failwith "declaration with no name?"
        in
        Some (name, declaration_constant.expr)
    | _ -> None
  in
  let constants = List.filter_map ast ~f:constant_declaration_extractor in
  let _, compiled =
    List.fold constants
      ~init:((fun (a : AST.expression) -> a), [])
      ~f:(fun (let_wrapper, declarations) (name, expression) ->
        let expr_var =
          {
            Simple_utils.Location.wrap_content = Simple_utils.Var.of_name name;
            location = Simple_utils.Location.Virtual "generated let";
          }
        in
        let let_wrapper, declaration =
          let compiled =
            tail_compile ~raise empty_environment (let_wrapper expression)
          in
          ( (fun a ->
              let_wrapper
                (make_expression_with_dependencies [ (expr_var, expression) ] a)),
            compiled )
        in
        (let_wrapper, (name, declaration) :: declarations))
  in
  compiled |> List.rev
