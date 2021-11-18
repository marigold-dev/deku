open Test_helpers

let get_program = get_program "./contracts/multisig-v2.ligo" (Contract "main")

let compile_main ~raise ~add_warning () =
  let typed_prg,_   = get_program ~raise ~add_warning () in
  let mini_c_prg    = Ligo_compile.Of_typed.compile ~raise typed_prg in
  let michelson_prg = Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~raise ~options mini_c_prg "main" in
  let _contract =
    (* fails if the given entry point is not a valid contract *)
    Ligo_compile.Of_michelson.build_contract michelson_prg in
  ()

open Ast_imperative

let empty_op_list =
  (e_typed_list [] (t_operation ()))
let empty_message = e_lambda_ez (Location.wrap @@ Var.of_name "arguments")
  ~ascr:(t_bytes ()) (Some (t_list (t_operation ())))
  empty_op_list
let empty_message2 = e_lambda_ez (Location.wrap @@ Var.of_name "arguments")
  ~ascr:(t_bytes ()) (Some (t_list (t_operation ())))
 ( e_let_in_ez (Location.wrap @@ Var.of_name "foo") ~ascr:(t_unit ()) [] (e_unit ()) empty_op_list)

let send_param msg = e_constructor "Send" msg
let withdraw_param = e_constructor "Withdraw" empty_message

type st_type = {
  state_hash : bytes ;
  threshold:int ;
  max_proposal:int ;
  max_msg_size:int ;
  id_counter_list: (int * int) list ;
  msg_store_list: (expression * expression) list ;
}
let storage {state_hash ; threshold ; max_proposal ; max_msg_size ; id_counter_list ; msg_store_list} =
  let auth_set,counter_store = List.fold_left
    ~f:(fun (auth_set,counter_st) (id,ctr) ->
      let addr_exp = e_address @@ addr id in
      addr_exp::auth_set , (addr_exp, e_nat ctr)::counter_st)
    ~init:([],[])
    id_counter_list in
  e_record_ez [
    ("state_hash"          , e_bytes_raw state_hash                                         ) ;
    ("threshold"           , e_nat threshold                                                ) ;
    ("max_proposal"        , e_nat max_proposal                                             ) ;
    ("max_message_size"    , e_nat max_msg_size                                             ) ;
    ("authorized_addresses", e_typed_set auth_set       (t_address ())                      ) ;
    ("message_store"       , e_typed_map msg_store_list (t_bytes ()) (t_set (t_address ())) ) ;
    ("proposal_counters"   , e_typed_map counter_store  (t_address ()) (t_nat ())           ) ;
  ]

(* sender not stored in the authorized set *)
let wrong_addr ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let init_storage = storage {
    threshold = 1 ; max_proposal = 1 ; max_msg_size = 1 ; state_hash = Bytes.empty ;
    id_counter_list = [1,0 ; 2,0] ;
    msg_store_list = []
  } in
  let sender = contract 3 in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  let () =
    let exp_failwith = "Unauthorized address" in
    expect_string_failwith ~raise ~options (program,env) "main"
    (e_pair (send_param empty_message) init_storage) exp_failwith in
  ()

(* send a message which exceed the size limit *)
let message_size_exceeded ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let init_storage = storage {
    threshold = 1 ; max_proposal = 1 ; max_msg_size = 1 ; state_hash = Bytes.empty ;
    id_counter_list = [1,0] ;
    msg_store_list = []
  } in
  let sender = contract 1 in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  let () =
    let exp_failwith = "Message size exceed maximum limit" in
    expect_string_failwith ~raise ~options (program,env) "main"
    (e_pair (send_param empty_message)  init_storage) exp_failwith in
  ()

(* sender has already has reached maximum number of proposal *)
let maximum_number_of_proposal ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let packed_payload1 = pack_payload ~raise env (send_param empty_message) in
  let bytes1 = e_bytes_raw packed_payload1 in
  let init_storage = storage {
    threshold = 1 ; max_proposal = 1 ; max_msg_size = 15 ; state_hash = Bytes.empty ;
    id_counter_list = [1,1] ;
    msg_store_list = [(bytes1, e_set [e_address@@ addr 1])]
  } in
  let sender = contract 1 in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  let () =
    let exp_failwith = "Maximum number of proposal reached" in
    expect_string_failwith ~raise ~options (program,env) "main"
      (e_pair (send_param empty_message2) init_storage) exp_failwith in
  ()

(* sender message is already stored in the message store *)
let send_already_accounted ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let packed_payload = pack_payload ~raise env empty_message in
  let bytes = e_bytes_raw packed_payload in
  let init_storage = storage {
    threshold = 2 ;  max_proposal = 1 ;  max_msg_size = 15 ; state_hash = Bytes.empty ;
    id_counter_list = [1,1 ; 2,0] ;
    msg_store_list = [(bytes, e_set [e_address@@ addr 1])]
  } in
  let options =
    let sender = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  expect_eq ~raise ~options (program,env) "main"
    (e_pair (send_param empty_message) init_storage) (e_pair empty_op_list init_storage)

(* sender message isn't stored in the message store *)
let send_never_accounted ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let packed_payload = pack_payload ~raise env empty_message in
  let bytes = e_bytes_raw packed_payload in
  let init_storage' = {
    threshold = 2 ; max_proposal = 1 ;  max_msg_size = 15 ; state_hash = Bytes.empty ;
    id_counter_list = [1,0 ; 2,0] ;
    msg_store_list = []
  } in
  let init_storage = storage init_storage' in
  let final_storage = storage { init_storage' with
    id_counter_list = [1,1 ; 2,0] ;
    msg_store_list = [(bytes, e_set [e_address@@ addr 1])] ;
  } in
  let options =
    let sender = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  expect_eq ~raise ~options (program,env) "main"
    (e_pair (send_param empty_message) init_storage) (e_pair empty_op_list final_storage)

(* sender withdraw message is already binded to one address in the message store *)
let withdraw_already_accounted_one ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let packed_payload = pack_payload ~raise env empty_message in
  let bytes = e_bytes_raw packed_payload in
  let param = withdraw_param in
  let init_storage' = {
    threshold = 2 ; max_proposal = 1 ;  max_msg_size = 1 ; state_hash = Bytes.empty ;
    id_counter_list = [1,1 ; 2,0] ;
    msg_store_list = [(bytes, e_set [e_address@@ addr 1])] ;
  } in
  let init_storage = storage init_storage' in
  let final_storage = storage { init_storage' with
    id_counter_list = [1,0 ; 2,0] ;
    msg_store_list = [] } in
  let options =
    let sender = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  expect_eq ~raise ~options (program,env) "main"
    (e_pair param init_storage) (e_pair empty_op_list final_storage)

(* sender withdraw message is already binded to two addresses in the message store *)
let withdraw_already_accounted_two ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let packed_payload = pack_payload ~raise env empty_message in
  let bytes = e_bytes_raw packed_payload in
  let param = withdraw_param in
  let init_storage' = {
    threshold = 2 ; max_proposal = 2 ;  max_msg_size = 1 ; state_hash = Bytes.empty ;
    id_counter_list = [1,1 ; 2,1] ;
    msg_store_list = [(bytes, e_set [e_address@@ addr 1; e_address@@ addr 2])] ;
  } in
  let init_storage = storage init_storage' in
  let final_storage = storage { init_storage' with
    id_counter_list = [1,0 ; 2,1] ;
    msg_store_list = [(bytes, e_set [e_address@@ addr 2])] } in
  let options =
    let sender = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  expect_eq ~raise ~options (program,env) "main"
    (e_pair param init_storage) (e_pair empty_op_list final_storage)

(* triggers the threshold and check that all the participants get their counters decremented *)
let counters_reset ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let packed_payload = pack_payload ~raise env empty_message in
  let bytes = e_bytes_raw packed_payload in
  let param = send_param empty_message in
  let hash_after_msg = sha_256_hash (Bytes.concat Bytes.empty [Bytes.empty ; packed_payload]) in
  let init_storage' = {
    threshold = 3 ; max_proposal = 2 ;  max_msg_size = 15 ; state_hash = Bytes.empty ;
    id_counter_list = [1,1 ; 2,1 ; 3,0] ;
    msg_store_list = [(bytes, e_set [e_address@@ addr 1; e_address@@ addr 2])] ;
  } in
  let init_storage = storage init_storage' in
  let final_storage = storage { init_storage' with
    state_hash = hash_after_msg ;
    id_counter_list = [1,0 ; 2,0 ; 3,0] ;
    msg_store_list = [] } in
  let options =
    let sender = contract 3 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  expect_eq ~raise ~options (program,env) "main"
    (e_pair param init_storage) (e_pair empty_op_list final_storage)

(* sender withdraw message was never accounted *)
let withdraw_never_accounted ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let param = withdraw_param in
  let init_storage = storage {
    threshold = 2 ; max_proposal = 1 ;  max_msg_size = 1 ; state_hash = Bytes.empty ;
    id_counter_list = [1,0 ; 2,0] ;
    msg_store_list = [] ;
  } in
  let options =
    let sender = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  expect_eq ~raise ~options (program,env) "main"
    (e_pair param init_storage) (e_pair empty_op_list init_storage)

(* successful storing in the message store *)
let succeeded_storing ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let packed_payload = pack_payload ~raise env empty_message in
  let bytes = e_bytes_raw packed_payload in
  let init_storage th = {
    threshold = th ; max_proposal = 1 ;  max_msg_size = 15 ; state_hash = Bytes.empty ;
    id_counter_list = [1,0 ; 2,0 ; 3,0] ;
    msg_store_list = [(bytes, e_typed_set [] (t_address ()))] ;
  } in
  let options =
    let sender = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  let () = expect_eq_n_trace_aux ~raise ~options [1;2] (program,env) "main"
      (fun th ->
        let init_storage = storage (init_storage th) in
        e_pair (send_param empty_message) init_storage
      )
      (fun th ->
        let hash_after_msg = sha_256_hash (Bytes.concat Bytes.empty [Bytes.empty ; packed_payload]) in
        let final_id_counter, final_msg_store, ret, final_state_hash = match th with
          | 1 -> [1,0 ; 2,0 ; 3,0] , []                                    , empty_op_list ,  hash_after_msg
          | 2 -> [1,1 ; 2,0 ; 3,0] , [(bytes, e_set [e_address@@ addr 1])] , empty_op_list , (init_storage th).state_hash
          | _ -> assert false in
        let final_storage = storage { (init_storage th) with
          state_hash = final_state_hash ;
          msg_store_list = final_msg_store ;
          id_counter_list = final_id_counter } in
        e_pair ret final_storage
      ) in
  ()

let main = test_suite "Multisig v2" [
    test_w "compile"                        (compile_main                  ) ;
    test_w "wrong_addr"                     (wrong_addr                    ) ;
    test_w "message_size_exceeded"          (message_size_exceeded         ) ;
    test_w "maximum_number_of_proposal"     (maximum_number_of_proposal    ) ;
    test_w "send_already_accounted"         (send_already_accounted        ) ;
    test_w "send_never_accounted"           (send_never_accounted          ) ;
    test_w "succeeded_storing"              (succeeded_storing             ) ;
    test_w "withdraw_already_accounted_one" (withdraw_already_accounted_one) ;
    test_w "withdraw_already_accounted_two" (withdraw_already_accounted_two) ;
    test_w "counters_reset"                 (counters_reset                ) ;
  ]
