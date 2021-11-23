open Simple_utils
open Trace
open Zinc_types
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
    raise:Errors.zincing_error raise -> environment -> AST.expression -> zinc =
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
    other_compile environment ~k:(Grab :: result_compiled) value
  in
  let compile_known_function_application =
    compile_known_function_application ~raise
  in
  let compile_function_application ~function_compiler environment expr args =
    compile_known_function_application environment
      (function_compiler environment expr)
      args
  in

  match expr.expression_content with
  | E_lambda lambda ->
      Grab
      ::
      tail_compile
        (environment |> add_binder lambda.binder.wrap_content)
        lambda.result
  | E_let_in { let_binder; rhs; let_result; _ } ->
      compile_let environment ~let':let_binder.wrap_content ~equal:rhs
        ~in':let_result
  (* TODO: function applications are disagregated in typed_ast, this defeats the whole purpose of using zinc, need to fix this *)
  | E_application { lamb; args } ->
      compile_function_application ~function_compiler:tail_compile environment
        lamb [ args ]
  | _ -> other_compile environment ~k:[ Return ] expr

(*** For optimization purposes, we have one function for compiling expressions in the "tail position" and another for 
     compiling everything else. *)
and other_compile :
    raise:Errors.zincing_error raise ->
    environment ->
    AST.expression ->
    k:zinc ->
    zinc =
 fun ~raise environment expr ~k ->
  let () =
    print_endline
      (Format.asprintf "other compile: %a / ~k:%s / env: %s" AST.PP.expression
         expr (show_zinc k)
         (environment.binders
         |> List.map ~f:(Format.asprintf "%a" Var.pp)
         |> String.concat ","))
  in
  let tail_compile = tail_compile ~raise in
  let other_compile = other_compile ~raise in
  let compile_pattern_matching = compile_pattern_matching ~raise in
  let compile_let environment ~let':name ~equal:value ~in':expression =
    let result_compiled =
      other_compile (environment |> add_binder name) expression ~k:(EndLet :: k)
    in
    other_compile environment ~k:(Grab :: result_compiled) value
  in
  let compile_known_function_application environment compiled_func args ~k =
    compile_known_function_application ~raise environment (compiled_func ~k)
      args
  in
  let compile_function_application ~function_compiler environment expr args ~k =
    PushRetAddr k
    ::
    compile_known_function_application environment
      (fun ~k -> function_compiler environment expr ~k:(Apply :: k))
      args ~k
  in
  match expr.expression_content with
  | E_literal literal -> (
      match literal with
      | Literal_int x -> Num x :: k
      | Literal_address s -> Address s :: k
      | Literal_bytes b -> Bytes b :: k
      | Literal_string (Standard b) -> String b :: k
      | Literal_string (Verbatim b) -> String b :: k
      | Literal_mutez a -> Mutez a :: k
      | Literal_key a -> Key a :: k
      | Literal_unit -> MakeRecord 0 :: k
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
      | Some index -> Access index :: k)
  (* TODO: function applications are disagregated in typed_ast, this defeats the whole purpose of using zinc, need to fix this *)
  | E_application { lamb; args } ->
      compile_function_application ~function_compiler:other_compile environment
        lamb [ args ] ~k
  | E_lambda { binder = { wrap_content = binder; _ }; result } ->
      Closure (Grab :: tail_compile (environment |> add_binder binder) result)
      :: k
  | E_recursive _recursive -> failwith "E_recursive unimplemented"
  | E_let_in { let_binder; rhs; let_result; _ } ->
      compile_let environment ~let':let_binder.wrap_content ~equal:rhs
        ~in':let_result
  | E_type_in _type_in -> failwith "E_type_in unimplemented"
  | E_mod_in _mod_in -> failwith "E_mod_in unimplemented"
  | E_mod_alias _mod_alias -> failwith "E_mod_alias unimplemented"
  | E_raw_code _raw_code -> failwith "E_raw_code unimplemented"
  (* Variant *)
  | E_constructor { constructor = Label constructor; element } ->
      compile_known_function_application environment
        (fun ~k -> MakeVariant (constructor) :: k)
        [ element ] ~k
  | E_matching matching -> compile_pattern_matching environment matching ~k
  (* Record *)
  | E_record expression_label_map ->
    let bindings = Stage_common.Types.LMap.bindings  expression_label_map
    in
      compile_known_function_application environment
        (fun ~k ->
          MakeRecord
            (List.length bindings)
          :: k)
        (List.map ~f:(fun (_, value) -> value) bindings)
        ~k
  | E_record_accessor { record; path = path } ->
     let rows = record.type_expression.type_content in
     let label =  match rows with 
     | T_record rows -> 
      let[@warning "-8"] Some (label, _) = 
        let bindings = LMap.bindings rows.content in
        List.findi bindings ~f:(fun _ (k, _) -> k = path ) in 
      label
     | T_constant _typ ->
       let Stage_common.Types.Label path = path in 
       int_of_string path
     | _ -> failwith "other" in
      compile_known_function_application environment
        (fun ~k -> RecordAccess label :: k)
        [ record ] ~k
  | E_record_update _record_update -> failwith "E_record_update unimplemented"
  | E_type_inst _ -> failwith "E_type_inst unimplemented"
  | E_module_accessor _module_access ->
      failwith "E_module_accessor unimplemented"

and compile_constant :
    raise:Errors.zincing_error raise ->
    AST.type_expression ->
    AST.constant ->
    k:zinc ->
    zinc =
 fun ~raise:_ _ constant ~k ->
  match constant.cons_name with
  | C_CHAIN_ID -> ChainID :: k
  | C_HASH_KEY -> HashKey :: k
  | C_EQ -> Eq :: k
  | C_ADD -> Add :: k
  | C_FAILWITH -> Failwith :: k
  | C_CONTRACT_OPT -> Contract_opt :: k
  | C_CALL -> MakeTransaction :: k
  | C_UNIT -> MakeRecord 0 :: k
  | C_NONE -> MakeRecord 0 :: MakeVariant ("None") :: k
  | C_SOME -> MakeVariant ("Some") :: k
  | C_CONS -> Cons :: k 
  | C_LIST_EMPTY -> Nil :: k
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
    zinc ->
    AST.expression list ->
    zinc =
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

and compile_pattern_matching :
    raise:Errors.zincing_error raise ->
    environment ->
    AST.matching ->
    k:zinc ->
    zinc =
 fun ~raise environment to_match ~k ->
  let other_compile = other_compile ~raise in
  let compile_type = compile_type ~raise in
  let compiled_type = compile_type to_match.matchee.type_expression in
  match (compiled_type, to_match.cases) with
  | T_tuple _, Match_record { fields = binders; body; _ } ->
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
                rhs = to_match.matchee;
                let_result = lettified;
                attr =
                  {
                    inline = false;
                    no_mutation = false;
                    view = false;
                    public = false;
                  };
              };
          type_expression = to_match.matchee.type_expression;
          location = loc;
        }
      in
      other_compile environment lettified ~k
  | T_option _, Match_variant { cases; _ } ->
      let code =
        Zinc_types.MatchVariant
          (List.map
             ~f:
               (fun {
                      constructor = Label label;
                      pattern = { wrap_content = pattern; _ };
                      body;
                    } ->
               (* We assume that the interpreter will put the matched value on the stack *)
               let compiled =
                 Grab
                 :: other_compile (add_binder pattern environment) body ~k:[]
               in
               (label, compiled))
             cases)
      in
      other_compile environment to_match.matchee ~k:(code :: k)
  | _ ->
      failwith
        (Format.asprintf
           "E_matching unimplemented. Need to implement matching for %a"
           Mini_c.PP.type_content
           (compile_type to_match.matchee.type_expression))

let compile_module :
    raise:Errors.zincing_error raise -> AST.module_fully_typed -> program =
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
              make_expression_with_dependencies [ (expr_var, expression) ]
                (let_wrapper a)),
            compiled )
        in
        (let_wrapper, (name, declaration) :: declarations))
  in
  compiled |> List.rev
