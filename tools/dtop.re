let directive_string_fn = fn_name =>
  switch (Hashtbl.find_opt(Toploop.directive_table, fn_name)) {
  | Some(Toploop.Directive_string(fn)) => fn
  | _ =>
    Printf.printf(
      "Custom top-level failed to load due to an internal error\nUnable to find directive %s\n",
      fn_name,
    );
    exit(-1);
  };

let use_output = command => directive_string_fn("use_output", command);
let load_dune_libs = () => use_output @@ "dune top | grep -v threads.cma";

let () = load_dune_libs();
let () = UTop.require(["reason.ocaml-migrate-parsetree", "menhirLib"]);

let () =
  try(Topdirs.dir_directory(Sys.getenv("OCAML_TOPLEVEL_PATH"))) {
  | Not_found => ()
  };

let () = UTop.require(["reason.easy_format", "reason"]);

let () = Reason_toploop.main();

let () = Reason_utop.init_reason();

let () = UTop_main.main();
