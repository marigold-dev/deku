open Deku_stdlib.N

let z = Alcotest.testable Z.pp_print Z.equal
let n = Alcotest.testable pp equal
let two = one + one
let three = of_z (Z.of_int 3) |> Option.get

let test_of_z_0 () =
  Alcotest.(check' (option n))
    ~msg:"of_z 0" ~expected:(Some zero) ~actual:(of_z Z.zero)

let test_of_z_1 () =
  Alcotest.(check' (option n))
    ~msg:"of_z 1" ~expected:(Some one) ~actual:(of_z Z.one)

let test_of_z_2 () =
  Alcotest.(check' (option n))
    ~msg:"of_z 2" ~expected:(Some two)
    ~actual:(of_z Z.(one + one))

let test_of_z_neg_1 () =
  Alcotest.(check' (option n))
    ~msg:"of_z (-1)" ~expected:None
    ~actual:(of_z Z.(minus_one))

let test_of_z_neg_2 () =
  Alcotest.(check' (option n))
    ~msg:"of_z (-2)" ~expected:None
    ~actual:(of_z Z.(zero - one - one))

let test_to_z_0 () =
  Alcotest.(check' z) ~msg:"to_z 0" ~expected:Z.zero ~actual:(to_z zero)

let test_to_z_1 () =
  Alcotest.(check' z) ~msg:"to_z 1" ~expected:Z.one ~actual:(to_z one)

let test_to_z_2 () =
  Alcotest.(check' z) ~msg:"to_z 2" ~expected:Z.(one + one) ~actual:(to_z two)

let test_equal_0_0 () =
  Alcotest.(check' bool) ~msg:"0 = 0" ~expected:true ~actual:(equal zero zero)

let test_equal_1_1 () =
  Alcotest.(check' bool) ~msg:"1 = 1" ~expected:true ~actual:(equal one one)

let test_equal_2_2 () =
  Alcotest.(check' bool) ~msg:"2 = 2" ~expected:true ~actual:(equal two two)

let test_equal_0_1 () =
  Alcotest.(check' bool) ~msg:"0 = 1" ~expected:false ~actual:(equal zero one)

let test_equal_0_2 () =
  Alcotest.(check' bool) ~msg:"0 = 2" ~expected:false ~actual:(equal zero two)

let test_equal_1_0 () =
  Alcotest.(check' bool) ~msg:"1 = 0" ~expected:false ~actual:(equal one zero)

let test_equal_2_0 () =
  Alcotest.(check' bool) ~msg:"2 = 0" ~expected:false ~actual:(equal two zero)

let test_equal_1_2 () =
  Alcotest.(check' bool) ~msg:"1 = 2" ~expected:false ~actual:(equal one two)

let test_equal_2_1 () =
  Alcotest.(check' bool) ~msg:"2 = 1" ~expected:false ~actual:(equal two one)

let test_compare_0_0 () =
  Alcotest.(check' int)
    ~msg:"compare 0 0" ~expected:0 ~actual:(compare zero zero)

let test_compare_1_1 () =
  Alcotest.(check' int) ~msg:"compare 1 1" ~expected:0 ~actual:(compare one one)

let test_compare_2_2 () =
  Alcotest.(check' int) ~msg:"compare 2 2" ~expected:0 ~actual:(compare two two)

let test_compare_0_1 () =
  Alcotest.(check' int)
    ~msg:"compare 0 1" ~expected:(-1) ~actual:(compare zero one)

let test_compare_0_2 () =
  Alcotest.(check' int)
    ~msg:"compare 0 2" ~expected:(-1) ~actual:(compare zero two)

let test_compare_1_0 () =
  Alcotest.(check' int)
    ~msg:"compare 1 0" ~expected:1 ~actual:(compare one zero)

let test_compare_2_0 () =
  Alcotest.(check' int)
    ~msg:"compare 2 0" ~expected:1 ~actual:(compare two zero)

let test_compare_1_2 () =
  Alcotest.(check' int)
    ~msg:"compare 1 2" ~expected:(-1) ~actual:(compare one two)

let test_compare_2_1 () =
  Alcotest.(check' int) ~msg:"compare 2 1" ~expected:1 ~actual:(compare two one)

let test_add_0_0 () =
  Alcotest.(check' n) ~msg:"0 + 0" ~expected:zero ~actual:(zero + zero)

let test_add_0_1 () =
  Alcotest.(check' n) ~msg:"0 + 1" ~expected:one ~actual:(zero + one)

let test_add_1_0 () =
  Alcotest.(check' n) ~msg:"1 + 0" ~expected:one ~actual:(one + zero)

let test_add_1_1 () =
  Alcotest.(check' n) ~msg:"1 + 1" ~expected:two ~actual:(one + one)

let test_add_0_2 () =
  Alcotest.(check' n) ~msg:" 0 + 2" ~expected:two ~actual:(zero + two)

let test_add_2_0 () =
  Alcotest.(check' n) ~msg:"2 + 0" ~expected:two ~actual:(two + zero)

let test_add_2_1 () =
  Alcotest.(check' n) ~msg:"2 + 1" ~expected:three ~actual:(two + one)

let test_add_1_2 () =
  Alcotest.(check' n) ~msg:"1 + 2" ~expected:three ~actual:(one + two)

let test_sub_0_0 () =
  Alcotest.(check' (option n))
    ~msg:"0 - 0" ~expected:(Some zero) ~actual:(zero - zero)

let test_sub_1_0 () =
  Alcotest.(check' (option n))
    ~msg:"1 - 0" ~expected:(Some one) ~actual:(one - zero)

let test_sub_0_1 () =
  Alcotest.(check' (option n)) ~msg:"0 - 1" ~expected:None ~actual:(zero - one)

let test_sub_1_1 () =
  Alcotest.(check' (option n))
    ~msg:"1 - 1" ~expected:(Some zero) ~actual:(one - one)

let test_sub_2_0 () =
  Alcotest.(check' (option n))
    ~msg:"2 - 0" ~expected:(Some two) ~actual:(two - zero)

let test_sub_0_2 () =
  Alcotest.(check' (option n)) ~msg:"0 - 2" ~expected:None ~actual:(zero - two)

let test_sub_2_1 () =
  Alcotest.(check' (option n))
    ~msg:"2 - 1" ~expected:(Some one) ~actual:(two - one)

let test_sub_1_2 () =
  Alcotest.(check' (option n)) ~msg:"1 - 2" ~expected:None ~actual:(one - two)

let test_sub_2_2 () =
  Alcotest.(check' (option n))
    ~msg:"2 - 2" ~expected:(Some zero) ~actual:(two - two)

let test_lt_0_0 () =
  Alcotest.(check' bool) ~msg:"0 < 0" ~expected:false ~actual:(zero < zero)

let test_lt_0_1 () =
  Alcotest.(check' bool) ~msg:"0 < 1" ~expected:true ~actual:(zero < one)

let test_lt_1_0 () =
  Alcotest.(check' bool) ~msg:"1 < 0" ~expected:false ~actual:(one < zero)

let test_lt_1_1 () =
  Alcotest.(check' bool) ~msg:"1 < 1" ~expected:false ~actual:(one < one)

let test_lt_0_2 () =
  Alcotest.(check' bool) ~msg:"0 < 2" ~expected:true ~actual:(zero < two)

let test_lt_1_2 () =
  Alcotest.(check' bool) ~msg:"1 < 2" ~expected:true ~actual:(one < two)

let test_lt_2_0 () =
  Alcotest.(check' bool) ~msg:"2 < 0" ~expected:false ~actual:(two < zero)

let test_lt_2_1 () =
  Alcotest.(check' bool) ~msg:"2 < 1" ~expected:false ~actual:(two < one)

let test_lt_2_2 () =
  Alcotest.(check' bool) ~msg:"2 < 2" ~expected:false ~actual:(two < two)

let run () =
  let open Alcotest in
  run "N"
    [
      ( "of_z",
        [
          test_case "of_z_0" `Quick test_of_z_0;
          test_case "of_z_1" `Quick test_of_z_1;
          test_case "of_z_2" `Quick test_of_z_2;
          test_case "of_z_neg_1" `Quick test_of_z_neg_1;
          test_case "of_z_neg_2" `Quick test_of_z_neg_2;
        ] );
      ( "to_z",
        [
          test_case "to_z_0" `Quick test_to_z_0;
          test_case "to_z_1" `Quick test_to_z_1;
          test_case "to_z_2" `Quick test_to_z_2;
        ] );
      ( "equal",
        [
          test_case "eq_0_0" `Quick test_equal_0_0;
          test_case "eq_1_1" `Quick test_equal_1_1;
          test_case "eq_2_2" `Quick test_equal_2_2;
          test_case "eq_0_1" `Quick test_equal_0_1;
          test_case "eq_0_2" `Quick test_equal_0_2;
          test_case "eq_1_0" `Quick test_equal_1_0;
          test_case "eq_2_0" `Quick test_equal_2_0;
          test_case "eq_1_2" `Quick test_equal_1_2;
          test_case "eq_2_1" `Quick test_equal_2_1;
        ] );
      ( "compare",
        [
          test_case "compare_0_0" `Quick test_compare_0_0;
          test_case "compare_1_1" `Quick test_compare_1_1;
          test_case "compare_2_2" `Quick test_compare_2_2;
          test_case "compare_0_1" `Quick test_compare_0_1;
          test_case "compare_0_2" `Quick test_compare_0_2;
          test_case "compare_1_0" `Quick test_compare_1_0;
          test_case "compare_2_0" `Quick test_compare_2_0;
          test_case "compare_1_2" `Quick test_compare_1_2;
          test_case "compare_2_1" `Quick test_compare_2_1;
        ] );
      ( "add",
        [
          test_case "add_0_0" `Quick test_add_0_0;
          test_case "add_0_1" `Quick test_add_0_1;
          test_case "add_1_0" `Quick test_add_1_0;
          test_case "add_1_1" `Quick test_add_1_1;
          test_case "add_0_2" `Quick test_add_0_2;
          test_case "add_2_0" `Quick test_add_2_0;
          test_case "add_2_1" `Quick test_add_2_1;
          test_case "add_1_2" `Quick test_add_1_2;
        ] );
      ( "sub",
        [
          test_case "sub_0_0" `Quick test_sub_0_0;
          test_case "sub_1_0" `Quick test_sub_1_0;
          test_case "sub_0_1" `Quick test_sub_0_1;
          test_case "sub_1_1" `Quick test_sub_1_1;
          test_case "sub_2_0" `Quick test_sub_2_0;
          test_case "sub_0_2" `Quick test_sub_0_2;
          test_case "sub_2_1" `Quick test_sub_2_1;
          test_case "sub_1_2" `Quick test_sub_1_2;
          test_case "sub_2_2" `Quick test_sub_2_2;
        ] );
      ( "lt",
        [
          test_case "lt_0_0" `Quick test_lt_0_0;
          test_case "lt_0_1" `Quick test_lt_0_1;
          test_case "lt_1_0" `Quick test_lt_1_0;
          test_case "lt_1_1" `Quick test_lt_1_1;
          test_case "lt_0_2" `Quick test_lt_0_2;
          test_case "lt_1_2" `Quick test_lt_1_2;
          test_case "lt_2_0" `Quick test_lt_2_0;
          test_case "lt_2_1" `Quick test_lt_2_1;
          test_case "lt_2_2" `Quick test_lt_2_2;
        ] );
    ]
