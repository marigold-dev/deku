module Michelson = Tezos_utils.Michelson

include Memory_proto_alpha
let init_environment = Init_proto_alpha.init_environment
let dummy_environment = Init_proto_alpha.dummy_environment


open Protocol
open Script_typed_ir
open Script_ir_translator
open Script_interpreter

module X = struct
  open Alpha_context
  open Script_tc_errors
  open Alpha_environment.Error_monad

  let rec stack_ty_eq
    : type ta tb ra rb. context -> int -> (ta, ra) stack_ty -> (tb, rb) stack_ty ->
      (((ta, ra) stack_ty, (tb, rb) stack_ty) eq * context) tzresult
    = fun ctxt lvl ta tb ->
      let dummy_loc  = 0 in (*TODO not sure ..*)
      match ta, tb with
      | Item_t (tva, ra, _), Item_t (tvb, rb, _) ->
        ty_eq ctxt dummy_loc tva tvb |>
        record_trace (Bad_stack_item lvl) >>? fun (Eq, ctxt) ->
        stack_ty_eq ctxt (lvl + 1) ra rb >>? fun (Eq, ctxt) ->
        (Ok (Eq, ctxt) : (((ta, ra) stack_ty, (tb, rb) stack_ty) eq * context) tzresult)
      | Bot_t, Bot_t -> Ok (Eq, ctxt)
      | _, _ -> error Bad_stack_length



  module Unparse_costs = Michelson_v1_gas.Cost_of.Unparsing

  type ex_typed_value =
    Ex_typed_value : ('a Script_typed_ir.ty * 'a) -> ex_typed_value

module Interp_costs = Michelson_v1_gas.Cost_of

end

open X_error_monad

let stack_ty_eq (type a ra b rb)
    ?(tezos_context = (dummy_environment ()).tezos_context)
    (a:(a, ra) stack_ty) (b:(b, rb) stack_ty) =
  alpha_wrap (X.stack_ty_eq tezos_context 0 a b) >>? fun (Eq, _) ->
  ok Eq

let ty_eq (type a b)
    ?(tezos_context = (dummy_environment ()).tezos_context)
    (a:a ty) (b:b ty)
  =
  alpha_wrap (Script_ir_translator.ty_eq tezos_context 0 a b) >>? fun (Eq, _) ->
  ok Eq

(* should not need lwt *)
let prims_of_strings michelson =
  let (michelson, errs) =
    Tezos_client_011_PtHangzH.Michelson_v1_macros.expand_rec michelson in
  match errs with
  | _ :: _ ->
    Lwt.return (Error errs)
  | [] ->
  Lwt.return
    (alpha_wrap
       (Michelson_v1_primitives.prims_of_strings
          (Tezos_micheline.Micheline.strip_locations michelson))) >>=? fun michelson ->
  return (Tezos_micheline.Micheline.root michelson)


let lazy_expr expr =
    let open Alpha_context in
    Script.lazy_expr expr

(*
let parse_michelson (type aft aft'.)
    ?(tezos_context = (dummy_environment ()).tezos_context)
    ~top_level michelson
    ?type_logger
    (bef:('a, 'b) Script_typed_ir.stack_ty) (aft:(aft, aft') Script_typed_ir.stack_ty)
  =
  prims_of_strings michelson >>=? fun michelson ->
  parse_instr
    ?type_logger
    top_level tezos_context
    michelson bef ~legacy:false >>=?? fun (j, _) ->
  match j with
  | Typed descr -> (
      Lwt.return (
        alpha_wrap (X.stack_ty_eq tezos_context 0 descr.aft aft) >>? fun (Eq, _) ->
        let descr : ('a, 'b, aft, aft') Script_ir_translator.descr = {descr with aft} in
        Ok descr
      )
    )
  | _ -> Lwt.return @@ error_exn (Failure "Typing instr failed")
  *)

let parse_michelson_fail (type aft aftr)
    ?(tezos_context = (dummy_environment ()).tezos_context)
    ~top_level michelson
    ?type_logger
    (bef:('a, 'b) Script_typed_ir.stack_ty) (aft:(aft, aftr) Script_typed_ir.stack_ty)
    : (('a, 'b, aft, aftr) descr, error trace) result Lwt.t
  =
  prims_of_strings michelson >>=? fun michelson ->
  parse_instr
    ?type_logger
    top_level tezos_context
    michelson bef ~legacy:false >>=?? fun (j, _) ->
  match j with
  | Typed descr -> (
      Lwt.return (
        alpha_wrap (X.stack_ty_eq tezos_context 0 descr.aft aft) >>? fun (Eq, _) ->
          let descr : (_, _, aft, aftr) descr = {descr with aft} in
          Ok descr
      )
    )
  | Failed { descr } ->
    Lwt.return (Ok (descr aft))

let parse_michelson_data
    ?(tezos_context = (dummy_environment ()).tezos_context)
    michelson ty =
  parse_data tezos_context ty michelson ~legacy:false ~allow_forged:true >>=?? fun (data, _) ->
  return data

let parse_michelson_ty
    ?(tezos_context = (dummy_environment ()).tezos_context)
    ?(allow_operation = true) ?(allow_contract = true) ?(allow_lazy_storage = true) ?(allow_ticket = true)
    michelson =
  Lwt.return @@ parse_ty tezos_context ~allow_operation michelson ~legacy:false ~allow_contract ~allow_lazy_storage ~ allow_ticket >>=?? fun (ty, _) ->
  return ty

let strings_of_prims michelson =
  let michelson = Tezos_micheline.Micheline.strip_locations michelson in
  let michelson = Michelson_v1_primitives.strings_of_prims michelson in
  Tezos_micheline.Micheline.root michelson

let unparse_michelson_data
    ?(tezos_context = (dummy_environment ()).tezos_context)
    ty value =
  unparse_data tezos_context
    Readable ty value >>=?? fun (michelson, _) ->
  return (strings_of_prims michelson)

let unparse_michelson_ty
    ?(tezos_context = (dummy_environment ()).tezos_context)
    ty =
  Lwt.return @@ Script_ir_translator.unparse_ty tezos_context ty >>=?? fun (michelson, _) ->
  return (strings_of_prims michelson)

type options = {
  tezos_context: Alpha_context.t ;
  source: Alpha_context.Contract.t ;
  payer: Alpha_context.Contract.t ;
  self: Alpha_context.Contract.t ;
  amount: Alpha_context.Tez.t ;
  chain_id: Environment.Chain_id.t ;
  balance : Alpha_context.Tez.t;
  now : Alpha_context.Script_timestamp.t;
}

let t_unit = Tezos_micheline.Micheline.(strip_locations (Prim (0, Michelson_v1_primitives.T_unit, [], [])))
let default_self =
  force_ok_alpha ~msg:"bad default self"
    (Alpha_context.Contract.of_b58check "KT1DUMMYDUMMYDUMMYDUMMYDUMMYDUMu2oHG")

(* fake bake a block in order to set the predecessor timestamp *)
let fake_bake tezos_context chain_id now =
  let tezos_context = (Alpha_context.finalize tezos_context).context in
  let ((_, header, hash), _, _) =
    force_lwt ~msg:("bad init"^__LOC__)
      (Init_proto_alpha.Context_init.init 1) in
  let contents = Init_proto_alpha.Context_init.contents ~priority:0 () in
  let protocol_data =
    let open! Alpha_context.Block_header in {
      contents ;
      signature = Tezos_crypto.Signature.zero ;
  } in
  let tezos_context =
    force_lwt ~msg:("bad block "^__LOC__)
      ((Protocol.Main.begin_construction
        ~chain_id
        ~predecessor_context:tezos_context
        ~predecessor_timestamp:((match Alpha_context.Timestamp.of_seconds_string (Z.to_string (Alpha_context.Script_timestamp.to_zint now)) with
                    | Some t -> t
                    | _ -> Stdlib.failwith "bad timestamp"))
        ~predecessor_fitness:header.fitness
        ~predecessor_level:header.level
        ~predecessor:hash
        ~timestamp:(match Alpha_context.Timestamp.of_seconds_string (Z.to_string (Z.add Z.one (Alpha_context.Script_timestamp.to_zint now))) with
                    | Some t -> t
                    | _ -> Stdlib.failwith "bad timestamp")
        ~protocol_data
        ())
      >>= fun x -> Lwt.return @@ Alpha_environment.wrap_tzresult x >>=? fun state ->
                      return state.ctxt) in
  tezos_context

let make_options
    ?(tezos_context = (dummy_environment ()).tezos_context)
    ?(now = Alpha_context.Script_timestamp.now (dummy_environment ()).tezos_context)
    ?(sender = (List.nth_exn (dummy_environment ()).identities 0).implicit_contract)
    ?(self = default_self)
    ?(parameter_ty = t_unit)
    ?(source = (List.nth_exn (dummy_environment ()).identities 1).implicit_contract)
    ?(amount = Alpha_context.Tez.one)
    ?(balance = Alpha_context.Tez.zero)
    ?(chain_id = Environment.Chain_id.zero)
    ()
  =
  let open Alpha_context in
  let open Michelson_v1_primitives in
  let open Tezos_micheline in
  let open Micheline in
  let dummy_script =
    let parameter_ty = root parameter_ty in
    Script.lazy_expr @@ strip_locations
    @@ Seq
         ( 0,
           [ Prim (0, K_parameter, [parameter_ty], []);
             Prim (0, K_storage, [Prim (0, T_unit, [], [])], []);
             Prim
               ( 0,
                 K_code,
                 [ Seq
                     ( 0,
                       [ Prim (0, I_CDR, [], []);
                         Prim (0, I_NIL, [Prim (0, T_operation, [], [])], []);
                         Prim (0, I_PAIR, [], []) ] ) ],
                 [] ) ] )
  in
  let dummy_storage =
    Micheline.strip_locations
    @@ Micheline.Prim (0, Michelson_v1_primitives.D_Unit, [], [])
  in
  let lazy_dummy_storage = Script.lazy_expr dummy_storage in
  let script = Script.{code = dummy_script; storage = lazy_dummy_storage} in
  let tezos_context =
    force_lwt_alpha ~msg:("bad options "^__LOC__)
      (Alpha_context.Contract.originate
        tezos_context
        self
        ~balance
        ~delegate:None
        ~script:(script, None)) in
  (* fake bake to set the predecessor timestamp *)
  let time_between_blocks = 1 in
  let tezos_context = fake_bake tezos_context chain_id (Script_timestamp.sub_delta now (Script_int_repr.of_int time_between_blocks)) in
  {
    tezos_context ;
    source = sender ;
    payer = source ;
    self ;
    amount ;
    chain_id ;
    balance ;
    now = Script_timestamp.now tezos_context ;
  }

let no_trace_logger = None

let interpret ?(options = make_options ()) (instr:('a, 'b, 'c, 'd) kdescr) bef : (_*_) tzresult Lwt.t  =
  let {
    tezos_context ;
    source ;
    self ;
    payer ;
    amount ;
    chain_id ;
    balance = _ ;
    now = _ ;
  } = options in
  let step_constants = { source ; self ; payer ; amount ; chain_id } in
  (* (EmptyCell, EmptyCell) feels wrong here ..*)
  Script_interpreter.step no_trace_logger tezos_context step_constants instr bef (EmptyCell, EmptyCell) >>=??
  fun (stack, _, _) -> return stack

let unparse_ty_michelson ty =
  Lwt.return @@ Script_ir_translator.unparse_ty (dummy_environment ()).tezos_context ty >>=??
  fun (n,_) -> return n

type typecheck_res =
  | Type_checked
  | Err_parameter | Err_storage | Err_contract
  | Err_gas
  | Err_unknown

let typecheck_contract contract =
  let contract' = Tezos_micheline.Micheline.strip_locations contract in
  let legacy = false in
  Script_ir_translator.typecheck_code ~legacy (dummy_environment ()).tezos_context contract' >>= fun x ->
  match x with
  | Ok _ -> return @@ contract
  | Error errs -> Lwt.return @@ Error (Alpha_environment.wrap_tztrace errs)

type 'a interpret_res =
  | Succeed of 'a
  | Fail of Script_repr.expr

let failure_interpret
    ?(options = make_options ())
    (instr:('a, 's, 'b, 'u) descr)
    (bef:'a)
    stackb : _ interpret_res tzresult Lwt.t =
  let {
    tezos_context ;
    source ;
    self ;
    payer ;
    amount ;
    chain_id ;
    balance = _ ;
    now = _ ;
  } = options in

  let descr = instr in
  let kinfo = {iloc = descr.loc ; kstack_ty = descr.bef} in
  let kinfoh = {iloc = descr.loc ; kstack_ty = descr.aft} in
  let kinstr = descr.instr.apply kinfo (IHalt kinfoh) in
  let kdescr = {
    kloc = descr.loc ;
    kbef = descr.bef ;
    kaft = descr.aft ;
    kinstr;
  } in
  let instr = kdescr in

  let step_constants = { source ; self ; payer ; amount ; chain_id } in
  Script_interpreter.step no_trace_logger tezos_context step_constants instr bef stackb >>= fun x ->
  match x with
  | Ok (s, _, _ctxt) -> return @@ Succeed s
  | Error errs ->
    match Alpha_environment.wrap_tztrace errs with 
      Alpha_environment.(Ecoproto_error ( (Reject (_, expr, _)))::_t) -> return @@ Fail expr (* This catches failwith errors *)
    | _ -> Lwt.return @@ Error (Alpha_environment.wrap_tztrace errs)

let pack (data_ty: 'a ty) (data: 'a) : bytes tzresult Lwt.t =
  pack_data (dummy_environment ()).tezos_context data_ty data >>=?? fun (packed,_) -> return packed

let strings_of_prims = Michelson_v1_primitives.strings_of_prims

let to_hex = fun michelson ->
  let michelson =
    X_error_monad.force_lwt ~msg:"Internal error: could not serialize Michelson"
      (prims_of_strings michelson) in
  let canonical = Tezos_micheline.Micheline.strip_locations michelson in
  let bytes = Data_encoding.Binary.to_bytes_exn Script_repr.expr_encoding canonical in
  Hex.of_bytes bytes
