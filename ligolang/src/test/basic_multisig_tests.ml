open Test_helpers

let file   = "./contracts/basic_multisig/multisig.ligo"
let mfile  = "./contracts/basic_multisig/multisig.mligo"
let refile = "./contracts/basic_multisig/multisig.religo"

let get_program f = get_program f (Contract "main")

let compile_main ~raise ~add_warning f () =
  let typed_prg,_ = get_program ~raise ~add_warning f () in
  let mini_c_prg    = Ligo_compile.Of_typed.compile ~raise typed_prg in
  let michelson_prg = Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~raise ~options mini_c_prg "main" in
  let _contract =
    (* fails if the given entry point is not a valid contract *)
    Ligo_compile.Of_michelson.build_contract michelson_prg in
  ()

open Ast_imperative

let init_storage threshold counter pkeys =
  let keys = List.map
    ~f:(fun el ->
      let (_,pk_str,_) = str_keys el in
      e_key @@ pk_str)
    pkeys in
  e_record_ez [
    ("id" , e_string "MULTISIG" ) ;
    ("counter" , e_nat counter ) ;
    ("threshold" , e_nat threshold) ;
    ("auth" , e_typed_list keys (t_key ())) ;
  ]

let (first_owner , first_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (dummy_environment ()).identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt


let op_list ~raise =
  let open Memory_proto_alpha.Protocol.Alpha_context in
  let open Proto_alpha_utils in
  let source =
    Trace.trace_alpha_tzresult ~raise (fun _ -> Main_errors.test_internal __LOC__) @@
    (Contract.of_b58check "KT1DUMMYDUMMYDUMMYDUMMYDUMMYDUMu2oHG") in
  let operation = 
    let parameters : Script.lazy_expr = Script.unit_parameter in
    let entrypoint = "default" in
    let destination = 
      Trace.trace_alpha_tzresult ~raise (fun _ -> Main_errors.test_internal __LOC__) @@
       Contract.of_b58check "tz1PpDGHRXFQq3sYDuH8EpLWzPm5PFpe1sLE"
    in
    Transaction {amount=Tez.zero; parameters; entrypoint; destination} in
  let opbytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.internal_operation_encoding
      (Internal_operation {source; operation; nonce=0}) in
  (e_typed_list [e_literal (Literal_operation opbytes)] (t_operation ()))
let empty_payload = e_unit ()

let chain_id_zero =
  e_bytes_raw (Tezos_crypto.Chain_id.to_bytes Tezos_base__TzPervasives.Chain_id.zero)

(* sign the message 'msg' with 'keys', if 'is_valid'=false the providid signature will be incorrect *)
let params ~raise ~add_warning counter payload keys is_validl f =
  let _,env   = get_program ~raise ~add_warning f () in
  let aux = fun acc (key,is_valid) ->
    let (_,_pk,sk) = key in
    let (pkh,_,_) = str_keys key in
    let msg = e_tuple
      [ payload ;
        e_nat counter ;
        e_string (if is_valid then "MULTISIG" else "XX") ;
        chain_id_zero ] in
    let signature = sign_message ~raise env msg sk in
    (e_pair (e_key_hash pkh) (e_signature signature))::acc in
  let signed_msgs = List.fold ~f:aux ~init:[] (List.rev @@ List.zip_exn keys is_validl) in
  e_record_ez [
      ("counter" , e_nat counter ) ;
      ("payload" , payload) ;
      ("signatures" , e_typed_list signed_msgs (t_pair (t_key_hash (),t_signature ())) ) ;
    ]

(* Provide one valid signature when the threshold is two of two keys *)
let not_enough_1_of_2 ~raise ~add_warning f () =
  let program = get_program ~raise ~add_warning f () in
  let exp_failwith = "Not enough signatures passed the check" in
  let keys = gen_keys () in
  let test_params = params ~raise ~add_warning 0 empty_payload [keys] [true] f in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~sender:first_contract () in
  let () = expect_string_failwith ~raise
    program ~options "main" (e_pair test_params (init_storage 2 0 [keys;gen_keys()])) exp_failwith in
  ()

let unmatching_counter ~raise ~add_warning f () =
  let program = get_program ~raise ~add_warning f () in
  let exp_failwith = "Counters does not match" in
  let keys = gen_keys () in
  let test_params = params ~raise ~add_warning 1 empty_payload [keys] [true] f in
  let () = expect_string_failwith ~raise
    program "main" (e_pair test_params (init_storage 1 0 [keys])) exp_failwith in
  ()

(* Provide one invalid signature (correct key but incorrect signature)
   when the threshold is one of one key *)
let invalid_1_of_1 ~raise ~add_warning f () =
  let program = get_program ~raise ~add_warning f () in
  let exp_failwith = "Invalid signature" in
  let keys = [gen_keys ()] in
  let test_params = params ~raise ~add_warning 0 empty_payload keys [false] f in
  let () = expect_string_failwith ~raise
    program "main" (e_pair test_params (init_storage 1 0 keys)) exp_failwith in
  ()

(* Provide one valid signature when the threshold is one of one key *)
let valid_1_of_1 ~raise ~add_warning f () =
  let program = get_program ~raise ~add_warning f () in
  let op_list = op_list ~raise in
  let keys = gen_keys () in
  let () = expect_eq_n_trace_aux ~raise [0;1;2] program "main"
      (fun n ->
        let params = params ~raise ~add_warning n empty_payload [keys] [true] f in
        e_pair params (init_storage 1 n [keys])
      )
      (fun n ->
        e_pair op_list (init_storage 1 (n+1) [keys])
      ) in
  ()

(* Provive two valid signatures when the threshold is two of three keys *)
let valid_2_of_3 ~raise ~add_warning f () =
  let program = get_program ~raise ~add_warning f () in
  let op_list = op_list ~raise in
  let param_keys = [gen_keys (); gen_keys ()] in
  let st_keys = param_keys @ [gen_keys ()] in
  let () = expect_eq_n_trace_aux ~raise [0;1;2] program "main"
      (fun n ->
        let params = params ~raise ~add_warning n empty_payload param_keys [true;true] f in
        e_pair params (init_storage 2 n st_keys)
      )
      (fun n ->
        e_pair op_list (init_storage 2 (n+1) st_keys)
      ) in
  ()

(* Provide one invalid signature and two valid signatures when the threshold is two of three keys *)
let invalid_3_of_3 ~raise ~add_warning f () =
  let program = get_program ~raise ~add_warning f () in
  let valid_keys = [gen_keys() ; gen_keys()] in
  let invalid_key = gen_keys () in
  let param_keys = valid_keys @ [invalid_key] in
  let st_keys = valid_keys @ [gen_keys ()] in
  let test_params = params ~raise ~add_warning 0 empty_payload param_keys [false;true;true] f in
  let exp_failwith = "Invalid signature" in
  let () = expect_string_failwith ~raise
    program "main" (e_pair test_params (init_storage 2 0 st_keys)) exp_failwith in
  ()

(* Provide two valid signatures when the threshold is three of three keys *)
let not_enough_2_of_3 ~raise ~add_warning f () =
  let program = get_program ~raise ~add_warning f () in
  let valid_keys = [gen_keys() ; gen_keys()] in
  let st_keys = gen_keys () :: valid_keys  in
  let test_params = params ~raise ~add_warning 0 empty_payload (valid_keys) [true;true] f in
  let exp_failwith = "Not enough signatures passed the check" in
  let () = expect_string_failwith ~raise
    program "main" (e_pair test_params (init_storage 3 0 st_keys)) exp_failwith in
  ()

let main = test_suite "Basic Multisig" [
    test_w "compile"                       (compile_main       file);
    test_w "unmatching_counter"            (unmatching_counter file);
    test_w "valid_1_of_1"                  (valid_1_of_1       file);
    test_w "invalid_1_of_1"                (invalid_1_of_1     file);
    test_w "not_enough_signature"          (not_enough_1_of_2  file);
    test_w "valid_2_of_3"                  (valid_2_of_3       file);
    test_w "invalid_3_of_3"                (invalid_3_of_3     file);
    test_w "not_enough_2_of_3"             (not_enough_2_of_3  file);
    test_w "compile (mligo)"               (compile_main       mfile);
    test_w "unmatching_counter (mligo)"    (unmatching_counter mfile);
    test_w "valid_1_of_1 (mligo)"          (valid_1_of_1       mfile);
    test_w "invalid_1_of_1 (mligo)"        (invalid_1_of_1     mfile);
    test_w "not_enough_signature (mligo)"  (not_enough_1_of_2  mfile);
    test_w "valid_2_of_3 (mligo)"          (valid_2_of_3       mfile);
    test_w "invalid_3_of_3 (mligo)"        (invalid_3_of_3     mfile);
    test_w "not_enough_2_of_3 (mligo)"     (not_enough_2_of_3  mfile);
    test_w "compile (religo)"              (compile_main       refile);
    test_w "unmatching_counter (religo)"   (unmatching_counter refile);
    test_w "valid_1_of_1 (religo)"         (valid_1_of_1       refile);
    test_w "invalid_1_of_1 (religo)"       (invalid_1_of_1     refile);
    test_w "not_enough_signature (religo)" (not_enough_1_of_2  refile);
    test_w "valid_2_of_3 (religo)"         (valid_2_of_3       refile);
    test_w "invalid_3_of_3 (religo)"       (invalid_3_of_3     refile);
    test_w "not_enough_2_of_3 (religo)"    (not_enough_2_of_3  refile);
  ]
