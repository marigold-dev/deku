open Test_helpers
open Trace
open Main_errors

type ('a,'err) sdata = {
  erroneous_source_file : string ;
  preproc :raise:'err raise -> string -> Buffer.t * (string * string) list;
  parser : raise:'err raise -> Buffer.t -> 'a
}

let pascaligo_sdata = {
  erroneous_source_file =
    "../passes/02-parsing/pascaligo/all.ligo" ;
  preproc =
    (fun ~raise s -> trace ~raise preproc_tracer @@
    fun ~raise -> Trace.from_result ~raise @@
    Preprocessing.Pascaligo.preprocess_string [] s);
  parser =
    fun ~raise buffer -> trace ~raise parser_tracer @@
      fun ~raise -> Parsing.Pascaligo.parse_expression buffer ~raise
}

let cameligo_sdata = {
  erroneous_source_file =
    "../passes/02-parsing/cameligo/all.mligo";
  preproc =
    (fun ~raise s -> trace ~raise preproc_tracer @@
    fun ~raise -> Trace.from_result ~raise @@
    Preprocessing.Cameligo.preprocess_string [] s);
  parser =
    fun ~raise buffer -> trace ~raise parser_tracer (
    Parsing.Cameligo.parse_expression buffer)
}

let reasonligo_sdata = {
  erroneous_source_file =
    "../passes/02-parsing/reasonligo/all.religo" ;
  preproc =
    (fun ~raise s -> trace ~raise preproc_tracer @@
    fun ~raise -> Trace.from_result ~raise @@
    Preprocessing.Reasonligo.preprocess_string [] s);
  parser =
    fun ~raise buffer -> trace ~raise parser_tracer (
    Parsing.Reasonligo.parse_expression buffer)
}

let get_exp_as_string filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let assert_syntax_error ~raise sdata () =
  let aux entry =
    Format.printf "Entry : <%s>%!\n" entry ;
    let c_unit,_ = sdata.preproc ~raise entry in
    Assert.assert_fail ~raise (test_internal __LOC__) @@ 
      sdata.parser c_unit;
    Format.printf "Parsed%!\n" ;
    ()
  in
  let exps = get_exp_as_string sdata.erroneous_source_file in
  List.iter ~f:aux exps


let () =
  Printexc.record_backtrace true ;
  run_test @@ test_suite "LIGO" [
    test_suite "Parser negative tests" [
      test "pascaligo"  @@ assert_syntax_error pascaligo_sdata ;
      test "cameligo"   @@ assert_syntax_error cameligo_sdata ;
      test "reasonligo" @@ assert_syntax_error reasonligo_sdata ;
    ]
  ] ;
  ()
