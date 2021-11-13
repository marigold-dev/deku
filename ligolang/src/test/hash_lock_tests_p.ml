open Trace
open Test_helpers
open Ast_imperative
open Main_errors


let get_program = get_program "./contracts/hashlock.ligo" (Contract "main")

let compile_main ~raise ~add_warning () =
  let typed_prg,_   = get_program ~raise ~add_warning () in
  let mini_c_prg    = Ligo_compile.Of_typed.compile ~raise typed_prg in
  let michelson_prg = Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~raise ~options mini_c_prg "main" in
  let _contract =
    (* fails if the given entry point is not a valid contract *)
    Ligo_compile.Of_michelson.build_contract ~raise michelson_prg in
  ()

let call msg = e_constructor "Call" msg
let mk_time ~raise st =
  match Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp.of_string st with
  | Some s -> s
  | None -> raise.raise @@ test_internal "bad timestamp notation"
let to_sec t = Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp.to_zint t
let storage hashed used commits =
  e_record_ez [("hashed", hashed);
               ("unused", e_bool used);
               ("commits", commits)]

let (first_committer , first_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (dummy_environment ()).identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let empty_op_list =
  (e_typed_list [] (t_operation ()))
let empty_message = e_lambda_ez (Location.wrap @@ Var.of_name "arguments")
  ~ascr:(t_unit ()) (Some (t_list (t_operation ())))
  empty_op_list


let commit ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let now = mk_time ~raise "2000-01-01T00:10:10Z" in
  let lock_time = mk_time ~raise "2000-01-02T00:10:10Z" in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let packed_sender = pack_payload ~raise env (e_address first_committer) in
  let salted_hash = e_bytes_raw (sha_256_hash
                                   (Bytes.concat Bytes.empty [test_hash_raw;
                                                              packed_sender]))

  in
  let pre_commits = e_typed_big_map [] (t_address ()) (t_record_ez [("date", (t_timestamp ()));
                                                              ("salted_hash", (t_bytes ()))])
  in
  let init_storage = storage test_hash true pre_commits in
  let commit =
    e_record_ez [("date", e_timestamp_z (to_sec lock_time));
                 ("salted_hash", salted_hash)]
  in
  let post_commits = e_big_map [((e_address first_committer), commit)]
  in
  let post_storage = storage test_hash true post_commits in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~now
      ~sender:first_contract
      ()
  in
  expect_eq ~raise ~options (program,env) "commit"
    (e_pair salted_hash init_storage) (e_pair empty_op_list post_storage)

(* Test that the contract fails if we haven't committed before revealing the answer *)
let reveal_no_commit ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let empty_message = empty_message in
  let reveal = e_record_ez [("hashable", e_bytes_string "hello world");
                            ("message", empty_message)]
  in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let pre_commits = e_typed_big_map [] (t_address ()) (t_record_ez [("date", (t_timestamp ()));
                                                              ("salted_hash", (t_bytes ()))])
  in
  let init_storage = storage test_hash true pre_commits in
  expect_string_failwith ~raise (program,env) "reveal"
    (e_pair reveal init_storage)
    "You have not made a commitment to hash against yet."

(* Test that the contract fails if our commit isn't 24 hours old yet *)
let reveal_young_commit ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let empty_message = empty_message in
  let reveal = e_record_ez [("hashable", e_bytes_string "hello world");
                            ("message", empty_message)]
  in
  let now = mk_time ~raise "2000-01-01T00:10:10Z" in
  let lock_time = mk_time ~raise "2000-01-02T00:10:10Z" in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let packed_sender = pack_payload ~raise env (e_address first_committer) in
  let salted_hash = e_bytes_raw (sha_256_hash
                                   (Bytes.concat Bytes.empty [test_hash_raw;
                                                              packed_sender])) in
  let commit =
    e_record_ez [("date", e_timestamp_z (to_sec lock_time));
                 ("salted_hash", salted_hash)]
  in
  let commits = e_big_map [((e_address first_committer), commit)]
  in
  let init_storage = storage test_hash true commits in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~now
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~raise ~options (program,env) "reveal"
    (e_pair reveal init_storage)
    "It has not been 24 hours since your commit yet."

(* Test that the contract fails if our reveal doesn't meet our commitment *)
let reveal_breaks_commit ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let empty_message = empty_message in
  let reveal = e_record_ez [("hashable", e_bytes_string "hello world");
                            ("message", empty_message)]
  in
  let now = mk_time ~raise "2000-01-01T00:10:10Z" in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let packed_sender = pack_payload ~raise env (e_address first_committer) in
  let salted_hash = e_bytes_raw (sha_256_hash
                                   (Bytes.concat Bytes.empty [Bytes.of_string "hello";
                                                              packed_sender])) in
  let commit =
    e_record_ez [("date", e_timestamp_z (to_sec now));
                 ("salted_hash", salted_hash)]
  in
  let commits = e_big_map [((e_address first_committer), commit)]
  in
  let init_storage = storage test_hash true commits in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~now
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~raise ~options (program,env) "reveal"
    (e_pair reveal init_storage)
    "This reveal does not match your commitment."

(* Test that the contract fails if we reveal the wrong bytes for the stored hash *)
let reveal_wrong_commit ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let empty_message = empty_message in
  let reveal = e_record_ez [("hashable", e_bytes_string "hello");
                            ("message", empty_message)]
  in
  let now = mk_time ~raise "2000-01-01T00:10:10Z" in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let packed_sender = pack_payload ~raise env (e_address first_committer) in
  let salted_hash = e_bytes_raw (sha_256_hash
                                   (Bytes.concat Bytes.empty [Bytes.of_string "hello";
                                                              packed_sender])) in
  let commit =
    e_record_ez [("date", e_timestamp_z (to_sec now));
                 ("salted_hash", salted_hash)]
  in
  let commits = e_big_map [((e_address first_committer), commit)]
  in
  let init_storage = storage test_hash true commits in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~now
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~raise ~options (program,env) "reveal"
    (e_pair reveal init_storage)
    "Your commitment did not match the storage hash."

(* Test that the contract fails if we try to reuse it after unused flag changed *)
let reveal_no_reuse ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let empty_message = empty_message in
  let reveal = e_record_ez [("hashable", e_bytes_string "hello");
                            ("message", empty_message)]
  in
  let now = mk_time ~raise "2000-01-01T00:10:10Z" in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let packed_sender = pack_payload ~raise env (e_address first_committer) in
  let salted_hash = e_bytes_raw (sha_256_hash
                                   (Bytes.concat Bytes.empty [Bytes.of_string "hello";
                                                              packed_sender])) in
  let commit =
    e_record_ez [("date", e_timestamp_z (to_sec now));
                 ("salted_hash", salted_hash)]
  in
  let commits = e_big_map [((e_address first_committer), commit)]
  in
  let init_storage = storage test_hash false commits in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~now
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~raise ~options (program,env) "reveal"
    (e_pair reveal init_storage)
    "This contract has already been used."

(* Test that the contract executes successfully with valid commit-reveal *)
let reveal ~raise ~add_warning () =
  let (program,env) = get_program ~raise ~add_warning () in
  let empty_message = empty_message in
  let reveal = e_record_ez [("hashable", e_bytes_string "hello world");
                            ("message", empty_message)]
  in
  let now = mk_time ~raise "2000-01-01T00:10:10Z" in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let packed_sender = pack_payload ~raise env (e_address first_committer) in
  let salted_hash = e_bytes_raw (sha_256_hash
                                   (Bytes.concat Bytes.empty [Bytes.of_string "hello world";
                                                              packed_sender])) in
  let commit =
    e_record_ez [("date", e_timestamp_z (to_sec now));
                 ("salted_hash", salted_hash)]
  in
  let commits = e_big_map [((e_address first_committer), commit)]
  in
  let init_storage = storage test_hash true commits in
  let post_storage = storage test_hash false commits in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~now
      ~sender:first_contract
      ()
  in
  expect_eq ~raise ~options (program,env) "reveal"
    (e_pair reveal init_storage) (e_pair empty_op_list post_storage)

let main = test_suite "Hashlock (PascaLIGO)" [
    test_w "compile"                               (compile_main) ;
    test_w "commit"                                (commit) ;
    test_w "reveal (raise.raise if no commitment)"        (reveal_no_commit) ;
    test_w "reveal (raise.raise if commit too young)"     (reveal_young_commit) ;
    test_w "reveal (raise.raise if breaks commitment)"    (reveal_breaks_commit) ;
    test_w "reveal (raise.raise if wrong bytes for hash)" (reveal_wrong_commit) ;
    test_w "reveal (raise.raise if attempt to reuse)"     (reveal_no_reuse) ;
    test_w "reveal"                                (reveal) ;
]
