module PP = PP
module Errors = Errors
module To_yojson = To_yojson
module Formatter = Formatter

open Types
module List = Simple_utils.List

module type M =
  sig
    type file_name = string
    type module_name = string
    type compilation_unit
    type meta_data
    val preprocess : file_name -> compilation_unit * meta_data * (file_name * module_name) list
    module AST : sig
      type declaration
      type t = declaration list
      type environment
      val add_to_env : environment -> module_name -> environment -> environment
      val init_env : environment
      val make_module_declaration : module_name -> t -> declaration
      val make_module_alias : module_name -> file_name -> declaration
    end
    val compile : AST.environment -> file_name -> meta_data -> compilation_unit -> (AST.t * AST.environment)
  end

module Make (M : M) =
  struct

  type file_name = M.file_name
  type vertice = M.meta_data * M.compilation_unit * (M.file_name * M.module_name) list
  type graph = G.t * vertice SMap.t
  type error = Errors.t
  type ast = M.AST.t 
  type env = M.AST.environment
  type 'a build_error = ('a, error) result



(* Build system *)

  let dependency_graph : file_name -> graph =
    fun file_name ->
    let vertices = SMap.empty in
    let dep_g = G.empty in
    let rec dfs acc (dep_g,vertices) (file_name,_module_name) =
      if not @@ SMap.mem file_name vertices then
        let c_unit, meta_data, deps = M.preprocess file_name in
        let vertices = SMap.add file_name (meta_data,c_unit,deps) vertices in
        let dep_g = G.add_vertex dep_g file_name in
        let dep_g =
          (* Don't add a loop on the first element *)
          if String.equal acc file_name then dep_g
          else G.add_edge dep_g acc file_name
        in
        let dep_g,vertices = List.fold ~f:(dfs file_name) ~init:(dep_g,vertices) deps in
        (dep_g,vertices)
      else
        let dep_g = G.add_edge dep_g acc file_name in
        (dep_g,vertices)
    in
    dfs file_name (dep_g,vertices) @@ (file_name,file_name)

  let solve_graph : graph -> file_name -> ((file_name * vertice) list,error) result =
    fun (dep_g,vertices) file_name ->
    if Dfs.has_cycle dep_g
    then (
      let graph = Format.asprintf "%a" PP.graph (dep_g,file_name) in
      Error (Errors.build_dependency_cycle graph)
    )
    else
      let aux v order =
        let elem = SMap.find v vertices in
        (v,elem)::order
      in
      let order = TopSort.fold aux dep_g [] in
      Ok (order)

  let aggregate_dependencies_as_headers order_deps asts_typed =
    (* Add the module at the beginning of the file *)
    let aux map (file_name,(_,_,deps_lst)) =
      let (ast,_) =
        match (SMap.find_opt file_name asts_typed) with
          Some ast -> ast
        | None -> failwith "failed to find module"
      in

      let aux ast_typed (file_name,module_name) =
        (M.AST.make_module_alias module_name file_name)
        :: ast_typed
      in
      let ast_typed = List.fold_left ~f:aux ~init:ast deps_lst in
      let map = SMap.add file_name ast_typed map in
      map
    in
    let asts_typed = List.fold ~f:aux ~init:SMap.empty order_deps in
    (* Separate the program and the dependency (those are process differently) *)
    let (file_name,(_,_,_deps_lst)),order_deps = match List.rev order_deps with
      | [] -> failwith "compiling nothing"
      | hd::tl -> (hd,tl) in
    let contract =
      match (SMap.find_opt file_name asts_typed) with
        Some ast -> ast
      | None -> failwith "failed to find module"
    in
    (* Add all dependency at the beginning of the file *)
    let add_modules dep_types (file_name,(_,_, _deps_lst)) =
      let module_binder = file_name in
      (* Get the ast_type of the module *)
      let ast_typed =
        match (SMap.find_opt file_name asts_typed) with
          Some ast -> ast
        | None -> failwith "failed to find module"
      in
      (dep_types,(M.AST.make_module_declaration module_binder ast_typed))
    in
    let _,header_list = List.fold_map_right ~f:add_modules ~init:(SMap.empty) @@ order_deps in
    let aggregated = List.fold_left ~f:(fun c a ->  a::c) ~init:contract header_list in
    aggregated

  let add_modules_in_env (env : M.AST.environment) deps =
    let aux env (module_name, (_,ast_env)) = 
      M.AST.add_to_env env module_name ast_env
    in
    List.fold_left ~f:aux ~init:env deps

  let add_deps_to_env asts_typed (_file_name, (_meta,_c_unit,deps)) =
    let aux (file_name,module_name) =
      let ast_typed =
        match (SMap.find_opt file_name asts_typed) with
          Some (ast) -> ast
        | None -> failwith "File typed before dependency. The build system is broken, contact the devs"
      in
      (module_name, ast_typed)
    in
    let deps = List.map ~f:aux deps in
    let env_with_deps = add_modules_in_env M.AST.init_env deps in
    env_with_deps

  let compile_file_with_deps asts (file_name, (meta,c_unit,deps)) =
    let env_with_deps = add_deps_to_env asts (file_name, (meta,c_unit,deps)) in
    let ast,ast_env = M.compile env_with_deps file_name meta c_unit in
    SMap.add file_name (ast,ast_env) asts

  let compile_separate : file_name -> (ast * env) build_error =
    fun main_file_name ->
      let deps = dependency_graph main_file_name in
      match solve_graph deps main_file_name with
        Ok (ordered_deps) ->
      (* This assumes that there are no dependency cycles involving the main file.
          Dependency cycles are not supported anyway. *)
        let mains, ordered_deps_only = List.partition_tf ~f:(fun (this_file_name, _) -> String.equal this_file_name main_file_name) ordered_deps in
        let main = assert (List.length mains == 1); List.hd_exn mains in
        let asts_typed = List.fold ~f:(compile_file_with_deps) ~init:(SMap.empty) ordered_deps_only in
        let asts_typed = compile_file_with_deps asts_typed main in
        Ok (SMap.find main_file_name asts_typed)
      | Error e -> Error e

  let compile_combined : file_name -> (M.AST.declaration list * env) build_error =
    fun file_name ->
      let deps = dependency_graph file_name in
      match solve_graph deps file_name with
        Ok (order_deps) ->
        let asts_typed = List.fold ~f:(compile_file_with_deps) ~init:(SMap.empty) order_deps in
        let contract = aggregate_dependencies_as_headers order_deps asts_typed in
        Ok(contract, snd @@ SMap.find file_name asts_typed)
      | Error e -> Error e
  end    

