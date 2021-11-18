open Test_helpers
open Main_errors

open Ast_core.Combinators
module Core = Typesystem.Core
open Ast_core.Types
open Ast_core.Reasons
(* open Typesystem.Solver_types *)
open Trace
(* open Typer_common.Errors *)
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet

let mk p_ctor_tag p_ctor_args =  (wrap (Todo "unit test") @@ P_constant { p_ctor_tag ; p_ctor_args ; })
(* A bunch of arbitrary types (they only need to be distrinct type constructors without arguments, feel free to replace the contents if/when some of these types move to the stdlib and aren't built-in anymore). *)
let (int, unit, nat, string, bytes, mutez) = (mk C_int [], mk C_unit [], mk C_nat [], mk C_string [], mk C_bytes [], mk C_mutez [])
(* An arbitrary two-argument type constructor (this only needs to be a type constructor with two arguments, feel free to replace). *)
let map (k,v) = mk C_map [k; v]
(* A bunch of type variables: *)
let (m,n,o,p,x,y,z) = let v name = Var.fresh ~name () in v "m", v "n", v "o", v "p", v "x", v "y", v "z"

let test_restrict
    (name : string)
    (* Restriction function under test *)
    (restrict : raise:'a raise -> (type_variable -> type_variable) -> constructor_or_row -> c_typeclass_simpl -> c_typeclass_simpl)
    (* New info: a variable assignment constraint: *)
    tv (_eq : string) c_tag tv_list
    (* Initial typeclass constraint: *)
    args (_in : string) tc
    (* Intermediate step (not tested): *)
    (_intermediate : bool list)
    (* Expected restricted typeclass:: *)
    expected_args (_in : string) expected_tc =
  test name @@ fun ~raise () ->
  let repr = (fun v -> v) in
    trace ~raise inference_tracer @@
    fun ~raise ->
    let info = `Constructor { reason_constr_simpl = "unit test" ; original_id = None; id_constructor_simpl = ConstraintIdentifier.T 42L ; tv ; c_tag ; tv_list } in
    let tc = make_c_typeclass_simpl ~bound:[] ~constraints:[] () 42 None args tc in
    let expected =  make_c_typeclass_simpl ~bound:[] ~constraints:[] () 42 None expected_args expected_tc in
    (* TODO: use an error not an assert *)
    (* Format.printf "\n\nActual: %a\n\n" Ast_typed.PP_generic.c_typeclass_simpl (restrict info tc);
     * Format.printf "\n\nExpected %a\n\n" Ast_typed.PP_generic.c_typeclass_simpl expected; *)
    let restricted = restrict ~raise repr info tc in
    Assert.assert_true ~raise (Typer_common.Errors.different_typeclasses expected restricted) (Ast_core.Compare.c_typeclass_simpl_compare_all_fields restricted expected = 0)


let tests1 restrict = [
  (
  test_restrict "restrict1" restrict
    (* New info: a variable assignment constraint: *)
    x "=" C_nat[]
    (* Initial typeclass constraint: *)
    [x;y;z] "∈" [[int ; unit ; unit] ; [nat ; int ; int] ; [nat ; int ; string] ; ]
    (* Intermediate step (not tested): *)
    (* *)        [ false              ;  true             ;  true                ; ]
    (* Expected restricted typeclass: *)
    [x;y;z] "∈" [                      [nat ; int ; int] ; [nat ; int ; string] ; ]
);

(  test_restrict "restrict2" restrict
    (* New info: a variable assignment constraint: *)
    x "=" C_map[m;n]
    (* Initial typeclass constraint: *)
    [x;y]   "∈" [[int  ; unit] ; [map(nat,nat)   ; int] ; [map(nat,string)   ; int] ; ]
    (* Intermediate step (not tested): *)
    (* *)        [ false        ;  true                  ; true                      ; ]
    (* Expected restricted typeclass constraint: *)
    [x;y]   "∈" [                [map(nat,nat)   ; int] ; [map(nat,string)   ; int] ; ]
)  ;

(  test_restrict "restrict3" restrict
    (* New info: a variable assignment constraint: *)
    y "=" C_int[]
    (* Initial typeclass constraint: *)
    [x;y;z] "∈" [[int ; unit ; unit] ; [nat ; int ; int] ; [nat ; int ; string] ; ]
    (* Intermediate step (not tested): *)
    (* *)        [false               ; true              ; true                 ; ]
    (* Expected restricted typeclass: *)
    [x;y;z] "∈" [                      [nat ; int ; int] ; [nat ; int ; string] ; ]
)  ;    
]

let test_deduce_and_clean
    name
    (deduce_and_clean : raise:'a raise -> (type_variable -> type_variable) -> c_typeclass_simpl -> _)
    repr
    args (_in : string) tc
    (expected_inferred  : (type_variable * constant_tag * type_variable list) list)
    expected_args (_in : string) expected_tc =
  test name @@ fun ~raise () ->
    trace ~raise inference_tracer @@
      fun ~raise ->
      let input_tc = make_c_typeclass_simpl ~bound:[] ~constraints:[] () 42 None args tc in
      let expected_tc = make_c_typeclass_simpl ~bound:[] ~constraints:[] () 42 None expected_args expected_tc in
      let expected_inferred = List.map
          ~f:(fun (tv , c_tag , tv_list) -> `Constructor {reason_constr_simpl = "unit test" ; original_id = None; id_constructor_simpl = ConstraintIdentifier.T 42L ; tv ; c_tag ; tv_list})
          expected_inferred in
      let actual = deduce_and_clean ~raise repr input_tc in
      Heuristic_tc_fundep_tests_compare_cleaned.compare_and_check_vars_deduce_and_clean_result ~raise { deduced = expected_inferred ; cleaned = expected_tc ; changed = true } actual

let inferred v (_eq : string) c args = v, c, args
let tests2 deduce_and_clean =
  let repr : type_variable ->type_variable = (fun v -> v) in
  [
  test_deduce_and_clean "deduce_and_clean split type constructor" deduce_and_clean repr
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [ map( nat , unit ) ; int ] ; [ map( bytes , mutez ) ; string ] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_map[m;n] ; ]
    (* Expected cleaned typeclass: *)
    [m;n;z] "∈" [ [      nat ; unit   ; int ] ; [      bytes ; mutez   ; string ] ; ]
  ;

  test_deduce_and_clean "deduce_and_clean recursive" deduce_and_clean repr
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [ map( nat , unit ) ; int ] ; [ map( bytes , unit ) ; string ] ; ]
    (* Expected inferred constraints: *)
    [ inferred x "=" C_map[m;n] ; 
      inferred n "=" C_unit[]   ; ]
    (* Expected cleaned typeclass: *)
    [m;z]   "∈" [ [      nat ;          int ] ; [      bytes ;          string ] ; ]
  ;

  test_deduce_and_clean "deduce_and_clean remove recursive" deduce_and_clean repr
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [ map( nat , unit ) ; int ] ; [ map( nat , unit ) ; string ] ; ]
    (* Expected inferred constraints: *)
    [ inferred x "=" C_map[m;n] ;
      inferred m "=" C_nat[]    ;
      inferred n "=" C_unit[]   ; ]
    (* Expected cleaned typeclass: *)
    [z]     "∈" [ [                     int ] ; [                     string ] ; ]
  ;

  test_deduce_and_clean "deduce_and_clean remove no-argument type constructor" deduce_and_clean repr
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [nat ; int] ; [nat ; string] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_nat[] ; ]
    (* Expected cleaned typeclass: *)
    [z]     "∈" [ [      int] ; [      string] ; ]
  ;

  test_deduce_and_clean "deduce_and_clean remove two no-argument type constructors" deduce_and_clean repr
    (* Input restricted typeclass: *)
    [x;y;z] "∈" [ [nat ; int ; unit] ; [nat ; string ; unit] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_nat[] ;
     inferred z "=" C_unit[] ; ]
    (* Expected cleaned typeclass: *)
    [y]     "∈" [ [      int       ] ; [      string       ] ; ]
  ;

  test_deduce_and_clean "deduce_and_clean split type constructor (again)" deduce_and_clean repr
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [map(nat,unit) ; int] ; [map(unit,nat) ; string] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_map[m;n] ; ]
    (* Expected cleaned typeclass: *)
    [m;n;z] "∈" [ [    nat;unit  ; int] ; [    unit;nat  ; string] ; ]
  ;

  test_deduce_and_clean "deduce_and_clean two recursive" deduce_and_clean repr
    (* Input restricted typeclass: *)
    [x;y;z]   "∈" [ [ map( nat , unit ) ; map( bytes , mutez ) ; int ] ; [ map( nat , unit ) ; map( bytes , unit ) ; string ] ; ]
    (* Expected inferred constraints: *)
    [ inferred x "=" C_map[m;n] ; 
      inferred m "=" C_nat[]    ;
      inferred n "=" C_unit[]   ;
      inferred y "=" C_map[o;p] ; 
      inferred o "=" C_bytes[]  ; 
    ]
    (* Expected cleaned typeclass: *)
    [p;z]     "∈" [ [                                  mutez   ; int ] ; [                                   unit   ; string ] ; ]
  ;
]

let main = test_suite "Typer: fundep heuriscic"
  @@ List.concat
    [
      tests1 @@ Inference.Heuristic_tc_fundep.restrict ;
      tests2 @@ Inference.Heuristic_tc_fundep.deduce_and_clean ;
    ]
