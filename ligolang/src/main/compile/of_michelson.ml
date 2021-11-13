open Main_errors
open Tezos_utils
open Proto_alpha_utils
open Trace

(* should preserve locations, currently wipes them *)
let build_contract ~raise :
  ?disable_typecheck:bool ->
  Stacking.compiled_expression ->
  (string * Stacking.compiled_expression) list -> _ Michelson.michelson  =
    fun ?(disable_typecheck= false) compiled views ->
      let open Tezos_micheline.Micheline in
      let views =
        List.map
          ~f:(fun (name, view) ->
            let (view_param_ty, ret_ty) = trace_option ~raise (entrypoint_not_a_function) @@ (* remitodo error specific to views*)
              Self_michelson.fetch_views_ty view.expr_ty
            in
            let view_param_ty = inject_locations (fun _ -> ()) (strip_locations view_param_ty) in
            let ret_ty = inject_locations (fun _ -> ()) (strip_locations ret_ty) in
            let view = inject_locations (fun _ -> ()) (strip_locations view.expr) in
            (name, view_param_ty, ret_ty, view)
          )
          views
      in
      let (param_ty, storage_ty) = trace_option ~raise (entrypoint_not_a_function) @@
        Self_michelson.fetch_contract_ty_inputs compiled.expr_ty in
      let param_ty = inject_locations (fun _ -> ()) (strip_locations param_ty) in
      let storage_ty = inject_locations (fun _ -> ()) (strip_locations storage_ty) in
      let expr = inject_locations (fun _ -> ()) (strip_locations compiled.expr) in
      let contract = Michelson.contract param_ty storage_ty expr views in
      if disable_typecheck then
        contract
      else
        let contract' =
          Trace.trace_tzresult_lwt ~raise (typecheck_contract_tracer contract)
            (Memory_proto_alpha.prims_of_strings contract) in
        let _ = Trace.trace_tzresult_lwt ~raise (typecheck_contract_tracer contract) @@
          Proto_alpha_utils.Memory_proto_alpha.typecheck_contract contract' in
        contract

let measure ~raise = fun m ->
  Trace.trace_tzresult_lwt ~raise (could_not_serialize) @@
    Proto_alpha_utils.Measure.measure m
