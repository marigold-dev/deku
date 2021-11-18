open Test_helpers

let file = "./contracts/multisig.ligo"
let mfile = "./contracts/multisig.mligo"
let refile = "./contracts/multisig.religo"

let get_program ~add_warning f = get_program ~add_warning f (Contract "main")

let compile_main ~raise ~add_warning f () =
  let typed_prg,_ = get_program ~raise ~add_warning f () in
  let mini_c_prg    = Ligo_compile.Of_typed.compile ~raise typed_prg in
  let michelson_prg = Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~raise ~options mini_c_prg "main" in
  let _contract =
    (* fails if the given entry point is not a valid contract *)
    Ligo_compile.Of_michelson.build_contract ~raise michelson_prg in
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

let empty_op_list =
  (e_typed_list [] (t_operation ()))

(* let empty_message = e_lambda (Location.wrap @@ Var.of_name "arguments",t_unit ())
  @@ e_annotation empty_op_list (t_list (t_operation ()))

let chain_id_zero =
  e_bytes_raw (Tezos_crypto.Chain_id.to_bytes Tezos_base__TzPervasives.Chain_id.zero) *)
let empty_message = e_lambda_ez (Location.wrap @@ Var.of_name "arguments")
  ~ascr:(t_unit ()) (Some (t_list (t_operation ())))
  empty_op_list
let chain_id_zero = e_chain_id @@ Tezos_crypto.Base58.simple_encode
  Tezos_base__TzPervasives.Chain_id.b58check_encoding
  Tezos_base__TzPervasives.Chain_id.zero

(* sign the message 'msg' with 'keys', if 'is_valid'=false the providid signature will be incorrect *)
let params ~raise ~add_warning counter msg keys is_validl f =
  let _,env = get_program ~raise ~add_warning f () in
  let aux = fun acc (key,is_valid) ->
    let (_,_pk,sk) = key in
    let (pkh,_,_) = str_keys key in
    let payload = e_tuple
      [ msg ;
        e_nat counter ;
        e_string (if is_valid then "MULTISIG" else "XX") ;
        chain_id_zero ] in
    let signature = sign_message ~raise env payload sk in
    (e_pair (e_key_hash pkh) (e_signature signature))::acc in
  let signed_msgs = List.fold ~f:aux ~init:[] (List.rev @@ List.zip_exn keys is_validl) in
  e_constructor
    "CheckMessage"
    (e_record_ez [
      ("counter" , e_nat counter ) ;
      ("message" , msg) ;
      ("signatures" , e_typed_list signed_msgs (t_pair (t_key_hash (),t_signature ())) ) ;
    ])

(* Provide one valid signature when the threshold is two of two keys *)
let not_enough_1_of_2 ~raise ~add_warning f () =
  let (program,env) = get_program ~raise ~add_warning f () in
  let exp_failwith = "Not enough signatures passed the check" in
  let keys = gen_keys () in
  let test_params = params ~raise ~add_warning 0 empty_message [keys] [true] f in
  let () = expect_string_failwith ~raise
    (program,env) "main" (e_pair test_params (init_storage 2 0 [keys;gen_keys()])) exp_failwith in
  ()

let unmatching_counter ~raise ~add_warning f () =
  let (program,env) = get_program ~raise ~add_warning f () in
  let exp_failwith = "Counters does not match" in
  let keys = gen_keys () in
  let test_params = params ~raise ~add_warning 1 empty_message [keys] [true] f in
  let () = expect_string_failwith ~raise
    (program,env) "main" (e_pair test_params (init_storage 1 0 [keys])) exp_failwith in
  ()

(* Provide one invalid signature (correct key but incorrect signature)
   when the threshold is one of one key *)
let invalid_1_of_1 ~raise ~add_warning f () =
  let (program,env) = get_program ~raise ~add_warning f () in
  let exp_failwith = "Invalid signature" in
  let keys = [gen_keys ()] in
  let test_params = params ~raise ~add_warning 0 empty_message keys [false] f in
  let () = expect_string_failwith ~raise
    (program,env) "main" (e_pair test_params (init_storage 1 0 keys)) exp_failwith in
  ()

(* Provide one valid signature when the threshold is one of one key *)
let valid_1_of_1 ~raise ~add_warning f () =
  let (program,env) = get_program ~raise ~add_warning f () in
  let keys = gen_keys () in
  let () = expect_eq_n_trace_aux ~raise [0;1;2] (program,env) "main"
      (fun n ->
        let params = params ~raise ~add_warning n empty_message [keys] [true] f in
        e_pair params (init_storage 1 n [keys])
      )
      (fun n ->
        e_pair empty_op_list (init_storage 1 (n+1) [keys])
      ) in
  ()

(* Provide two valid signatures when the threshold is two of three keys *)
let valid_2_of_3 ~raise ~add_warning f () =
  let (program,env) = get_program ~raise ~add_warning f () in
  let param_keys = [gen_keys (); gen_keys ()] in
  let st_keys = param_keys @ [gen_keys ()] in
  let () = expect_eq_n_trace_aux ~raise [0;1;2] (program,env) "main"
      (fun n ->
        let params = params ~raise ~add_warning n empty_message param_keys [true;true] f in
        e_pair params (init_storage 2 n st_keys)
      )
      (fun n ->
        e_pair empty_op_list (init_storage 2 (n+1) st_keys)
      ) in
  ()

(* Provide one invalid signature and two valid signatures when the threshold is two of three keys *)
let invalid_3_of_3 ~raise ~add_warning f () =
  let (program,env) = get_program ~raise ~add_warning f () in
  let valid_keys = [gen_keys() ; gen_keys()] in
  let invalid_key = gen_keys () in
  let param_keys = valid_keys @ [invalid_key] in
  let st_keys = valid_keys @ [gen_keys ()] in
  let test_params = params ~raise ~add_warning 0 empty_message param_keys [false;true;true] f in
  let exp_failwith = "Invalid signature" in
  let () = expect_string_failwith ~raise
    (program,env) "main" (e_pair test_params (init_storage 2 0 st_keys)) exp_failwith in
  ()

(* Provide two valid signatures when the threshold is three of three keys *)
let not_enough_2_of_3 ~raise ~add_warning f () =
  let (program,env) = get_program ~raise ~add_warning f () in
  let valid_keys = [gen_keys() ; gen_keys()] in
  let st_keys = gen_keys () :: valid_keys  in
  let test_params = params ~raise ~add_warning 0 empty_message (valid_keys) [true;true] f in
  let exp_failwith = "Not enough signatures passed the check" in
  let () = expect_string_failwith ~raise
    (program,env) "main" (e_pair test_params (init_storage 3 0 st_keys)) exp_failwith in
  ()

let main = test_suite "Multisig" [
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
