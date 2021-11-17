open Test_helpers

let ends_with suffix str =
  let str_len = String.length str in
  let suffix_len = String.length suffix in
  if str_len < suffix_len
  then false
  else
    String.equal suffix (String.sub str (str_len - suffix_len) suffix_len)

(* test that everything in src/test/contracts/positive typechecks and
   compiles (assuming entry point "main") *)
let positive_contract_tests =
  String.split_on_char ' ' (match Sys.getenv_opt "POSITIVE_CONTRACTS" with Some e -> e | None -> "") |>
  List.filter ~f:(fun path -> not (ends_with ".md" path)) |>
  List.map
    ~f:(fun path ->
      let run ~raise ~add_warning () =
        let prog = Ligo_compile.Utils.type_file ~raise ~add_warning ~options path "auto" Env in
        let _michelson = typed_program_to_michelson ~raise prog "main" in
        () in
        test_w ("src/test/"^path) run)

let main = test_suite "Positive contracts" (positive_contract_tests)
