open Test_helpers
open Ast_imperative



let get_program = get_program "./contracts/id.ligo" (Contract "main")

let compile_main ~raise ~add_warning () =
  let typed_prg,_   = get_program ~raise ~add_warning () in
  let mini_c_prg      = Ligo_compile.Of_typed.compile ~raise typed_prg in
  let michelson_prg   = Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~raise ~options mini_c_prg "main" in
  let _contract =
    (* fails if the given entry point is not a valid contract *)
    Ligo_compile.Of_michelson.build_contract ~raise michelson_prg in
  ()

let (first_owner , first_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (dummy_environment ()).identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let buy_id ~raise ~add_warning () =
  let program, env = get_program ~raise ~add_warning () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let storage = e_record_ez [("identities", (e_big_map [(e_int 0, id_details_1)])) ;
                             ("next_id", e_int 1) ;
                             ("name_price", e_mutez 1000000) ;
                             ("skip_price", e_mutez 1000000) ; ]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.one) ()
  in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", owner_website)]
  in
  let param = e_record_ez [("profile", owner_website) ;
                           ("initial_controller", (e_some (e_address new_addr))) ;
                          ] in
  let new_storage = e_record_ez [("identities", (e_big_map
                                                   [(e_int 0, id_details_1) ;
                                                    (e_int 1, id_details_2)])) ;
                                 ("next_id", e_int 2) ;
                                 ("name_price", e_mutez 1000000) ;
                                 ("skip_price", e_mutez 1000000) ; ]
  in
  let () = expect_eq ~raise ~options (program, env) "buy" 
      (e_pair param storage) 
      (e_pair (e_list []) new_storage)
  in ()

let buy_id_sender_addr ~raise ~add_warning () =
  let program = get_program ~raise ~add_warning () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let storage = e_record_ez [("identities", (e_big_map [(e_int 0, id_details_1)])) ;
                             ("next_id", e_int 1) ;
                             ("name_price", e_mutez 1000000) ;
                             ("skip_price", e_mutez 1000000) ; ]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.one) ()
  in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", owner_website)]
  in
  let param = e_record_ez [("profile", owner_website) ;
                           ("initial_controller", (e_typed_none (t_address ())))] in
  let new_storage = e_record_ez [("identities", (e_big_map
                                                   [(e_int 0, id_details_1) ;
                                                    (e_int 1, id_details_2)])) ;
                                 ("next_id", e_int 2) ;
                                 ("name_price", e_mutez 1000000) ;
                                 ("skip_price", e_mutez 1000000) ; ]
  in
  let () = expect_eq ~raise ~options program "buy"
      (e_pair param storage)
      (e_pair (e_list []) new_storage)
  in ()

(* Test that contract fails if we attempt to buy an ID for the wrong amount *)
let buy_id_wrong_amount ~raise ~add_warning () =
  let program = get_program ~raise ~add_warning () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let storage = e_record_ez [("identities", (e_big_map [(e_int 0, id_details_1)])) ;
                             ("next_id", e_int 1) ;
                             ("name_price", e_mutez 1000000) ;
                             ("skip_price", e_mutez 1000000) ; ]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.fifty_cents) ()
  in
  let param = e_record_ez [("profile", owner_website) ;
                           ("initial_controller", (e_some (e_address new_addr)))] in
  let () = expect_string_failwith ~raise ~options program "buy"
      (e_pair param storage)
      "Incorrect amount paid."
  in ()

let update_details_owner ~raise ~add_warning () =
  let program = get_program ~raise ~add_warning () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let id_details_2_diff = e_record_ez [("owner", e_address new_addr) ;
                                       ("controller", e_address new_addr) ;
                                       ("profile", owner_website)] in
  let storage = e_record_ez [("identities", (e_big_map
                                                   [(e_int 0, id_details_1) ;
                                                    (e_int 1, id_details_2)])) ;
                                 ("next_id", e_int 2) ;
                                 ("name_price", e_mutez 1000000) ;
                                 ("skip_price", e_mutez 1000000) ; ]
  in
  let new_storage = e_record_ez [("identities", (e_big_map
                                                   [(e_int 0, id_details_1) ;
                                                    (e_int 1, id_details_2_diff)])) ;
                                 ("next_id", e_int 2) ;
                                 ("name_price", e_mutez 1000000) ;
                                 ("skip_price", e_mutez 1000000) ; ]
  in
  let details = owner_website in
  let param = e_record_ez [("id", e_int 1) ;
                       ("new_profile", e_some details) ;
                       ("new_controller", e_some (e_address new_addr))] in
  let () = expect_eq ~raise ~options program "update_details"
      (e_pair param storage)
      (e_pair (e_list []) new_storage)
  in ()

let update_details_controller ~raise ~add_warning () =
  let program = get_program ~raise ~add_warning () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = owner_website in
  let id_details_2 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let id_details_2_diff = e_record_ez [("owner", e_address owner_addr) ;
                                       ("controller", e_address owner_addr) ;
                                       ("profile", new_website)] in
  let storage = e_record_ez [("identities", (e_big_map
                                               [(e_int 0, id_details_1) ;
                                                (e_int 1, id_details_2)])) ;
                             ("next_id", e_int 2) ;
                             ("name_price", e_mutez 1000000) ;
                             ("skip_price", e_mutez 1000000) ; ]
  in
  let new_storage = e_record_ez [("identities", (e_big_map
                                                   [(e_int 0, id_details_1) ;
                                                    (e_int 1, id_details_2_diff)])) ;
                                 ("next_id", e_int 2) ;
                                 ("name_price", e_mutez 1000000) ;
                                 ("skip_price", e_mutez 1000000) ; ]
  in
  let details = e_bytes_string "ligolang.org" in
  let param = e_record_ez [("id", e_int 1) ;
                           ("new_profile", e_some details) ;
                           ("new_controller", e_some (e_address owner_addr))] in
  let () = expect_eq ~raise ~options program "update_details"
      (e_pair param storage)
      (e_pair (e_list []) new_storage)
  in ()

(* Test that contract fails when we attempt to update details of nonexistent ID *)
let update_details_nonexistent ~raise ~add_warning () = 
  let program = get_program ~raise ~add_warning () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_record_ez [("identities", (e_big_map
                                                   [(e_int 0, id_details_1) ;
                                                    (e_int 1, id_details_2)])) ;
                                 ("next_id", e_int 2) ;
                                 ("name_price", e_mutez 1000000) ;
                                 ("skip_price", e_mutez 1000000) ; ]
  in
  let details = e_bytes_string "ligolang.org" in
  let param = e_record_ez [("id", e_int 2) ;
                           ("new_profile", e_some details) ;
                           ("new_controller", e_some (e_address owner_addr))] in
  let () = expect_string_failwith ~raise ~options program "update_details"
      (e_pair param storage)
      "This ID does not exist."
  in ()

(* Test that contract fails when we attempt to update details from wrong addr *)
let update_details_wrong_addr ~raise ~add_warning () =
  let program = get_program ~raise ~add_warning () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_record_ez [("identities", (e_big_map
                                                   [(e_int 0, id_details_1) ;
                                                    (e_int 1, id_details_2)])) ;
                                 ("next_id", e_int 2) ;
                                 ("name_price", e_mutez 1000000) ;
                                 ("skip_price", e_mutez 1000000) ; ]
  in
  let details = e_bytes_string "ligolang.org" in
  let param = e_record_ez [("id", e_int 0) ;
                           ("new_profile", e_some details) ;
                           ("new_controller", e_some (e_address owner_addr))] in
  let () = expect_string_failwith ~raise ~options program "update_details"
      (e_pair param storage)
      "You are not the owner or controller of this ID."
  in ()

(* Test that giving none on both profile and controller address is a no-op *)
let update_details_unchanged ~raise ~add_warning () =
  let program = get_program ~raise ~add_warning () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_record_ez [("identities", (e_big_map
                                               [(e_int 0, id_details_1) ;
                                                (e_int 1, id_details_2)])) ;
                             ("next_id", e_int 2) ;
                             ("name_price", e_mutez 1000000) ;
                             ("skip_price", e_mutez 1000000) ; ]
  in
  let param = e_record_ez [("id", e_int 1) ;
                           ("new_profile", e_typed_none (t_bytes ())) ;
                           ("new_controller", e_typed_none (t_address ()))] in
  let () = expect_eq ~raise ~options program "update_details"
      (e_pair param storage)
      (e_pair (e_list []) storage)
  in ()

let update_owner ~raise ~add_warning () =
  let program = get_program ~raise ~add_warning () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let id_details_2_diff = e_record_ez [("owner", e_address owner_addr) ;
                                       ("controller", e_address new_addr) ;
                                       ("profile", new_website)] in
  let storage = e_record_ez [("identities", (e_big_map
                                               [(e_int 0, id_details_1) ;
                                                (e_int 1, id_details_2)])) ;
                             ("next_id", e_int 2) ;
                             ("name_price", e_mutez 1000000) ;
                             ("skip_price", e_mutez 1000000) ; ]
  in
  let new_storage = e_record_ez [("identities", (e_big_map
                                               [(e_int 0, id_details_1) ;
                                                (e_int 1, id_details_2_diff)])) ;
                             ("next_id", e_int 2) ;
                             ("name_price", e_mutez 1000000) ;
                             ("skip_price", e_mutez 1000000) ; ]
  in
  let param = e_record_ez [("id", e_int 1) ;
                           ("new_owner", e_address owner_addr)] in
  let () = expect_eq ~raise ~options program "update_owner"
      (e_pair param storage)
      (e_pair (e_list []) new_storage)
  in ()

(* Test that contract fails when we attempt to update owner of nonexistent ID *)
let update_owner_nonexistent ~raise ~add_warning () =
  let program = get_program ~raise ~add_warning () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_record_ez [("identities", (e_big_map
                                               [(e_int 0, id_details_1) ;
                                                (e_int 1, id_details_2)])) ;
                             ("next_id", e_int 2) ;
                             ("name_price", e_mutez 1000000) ;
                             ("skip_price", e_mutez 1000000) ; ]
  in
  let param = e_record_ez [("id", e_int 2);
                           ("new_owner", e_address new_addr)] in
  let () = expect_string_failwith ~raise ~options program "update_owner"
      (e_pair param storage)
      "This ID does not exist."
  in ()

(* Test that contract fails when we attempt to update owner from non-owner addr *)
let update_owner_wrong_addr ~raise ~add_warning () =
  let program = get_program ~raise ~add_warning () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_record_ez [("identities", (e_big_map
                                               [(e_int 0, id_details_1) ;
                                                (e_int 1, id_details_2)])) ;
                             ("next_id", e_int 2) ;
                             ("name_price", e_mutez 1000000) ;
                             ("skip_price", e_mutez 1000000) ; ]
  in
  let param = e_record_ez [("id", e_int 0);
                           ("new_owner", e_address new_addr)] in
  let () = expect_string_failwith ~raise ~options program "update_owner"
      (e_pair param storage)
      "You are not the owner of this ID."
  in ()

let skip ~raise ~add_warning () =
  let program = get_program ~raise ~add_warning () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.one) ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_record_ez [("identities", (e_big_map
                                               [(e_int 0, id_details_1) ;
                                                (e_int 1, id_details_2)])) ;
                             ("next_id", e_int 2) ;
                             ("name_price", e_mutez 1000000) ;
                             ("skip_price", e_mutez 1000000) ; ]
  in
  let new_storage = e_record_ez [("identities", (e_big_map
                                               [(e_int 0, id_details_1) ;
                                                (e_int 1, id_details_2)])) ;
                             ("next_id", e_int 3) ;
                             ("name_price", e_mutez 1000000) ;
                             ("skip_price", e_mutez 1000000) ; ]
  in
  let () = expect_eq ~raise ~options program "skip_"
      (e_pair (e_unit ()) storage)
      (e_pair (e_list []) new_storage)
  in ()

(* Test that contract fails if we try to skip without paying the right amount *)
let skip_wrong_amount ~raise ~add_warning () =
  let program = get_program ~raise ~add_warning () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.fifty_cents) ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_record_ez [("identities", (e_big_map
                                               [(e_int 0, id_details_1) ;
                                                (e_int 1, id_details_2)])) ;
                             ("next_id", e_int 2) ;
                             ("name_price", e_mutez 1000000) ;
                             ("skip_price", e_mutez 1000000) ; ]
  in
  let () = expect_string_failwith ~raise ~options program "skip_"
      (e_pair (e_unit ()) storage)
      "Incorrect amount paid."
  in ()

let main = test_suite "ID Layer (PascaLIGO)" [
    test_w "buy"                         (buy_id ) ;
    test_w "buy (sender addr)"           (buy_id_sender_addr) ;
    test_w "buy (wrong amount)"          (buy_id_wrong_amount) ;
    test_w "update_details (owner)"      (update_details_owner) ;
    test_w "update_details (controller)" (update_details_controller) ;
    test_w "update_details_nonexistent"  (update_details_nonexistent) ;
    test_w "update_details_wrong_addr"   (update_details_wrong_addr) ;
    test_w "update_details_unchanged"    (update_details_unchanged ) ;
    test_w "update_owner"                (update_owner) ;
    test_w "update_owner_nonexistent"    (update_owner_nonexistent) ;
    test_w "update_owner_wrong_addr"     (update_owner_wrong_addr) ;
    test_w "skip"                        (skip) ;
    test_w "skip (wrong amount)"         (skip_wrong_amount) ;
]
