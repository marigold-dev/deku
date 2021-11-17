open Cli_expect

let%expect_test _ =
    run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/broken_string.ligo" ] ;
  [%expect {|
File "../../test/lexer/broken_string.ligo", line 1, characters 18-19:
  1 | const a: string = "broken
  2 | over
The string starting here is interrupted by a line break.
Hint: Remove the break, close the string before or insert a backslash.
 |} ];

    run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/broken_string.mligo" ] ;
  [%expect {|
File "../../test/lexer/broken_string.mligo", line 1, characters 8-9:
  1 | let a = "broken
  2 | over
The string starting here is interrupted by a line break.
Hint: Remove the break, close the string before or insert a backslash.
 |} ];

 run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/broken_string.religo" ] ;
  [%expect {|
File "../../test/lexer/broken_string.religo", line 1, characters 8-9:
  1 | let a = "broken
  2 | over
The string starting here is interrupted by a line break.
Hint: Remove the break, close the string before or insert a backslash.
 |} ];

(*
run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/negative_byte_sequence.ligo" ; "main" ] ;
  [%expect {|
File "../../test/lexer/negative_byte_sequence.ligo", line 1, characters 18-31:
  1 | const a: string = - (**) 0x2222
Negative byte sequence.
Hint: Remove the leading minus sign.
 |} ];

run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/negative_byte_sequence.mligo" ; "main" ] ;
  [%expect {|
File "../../test/lexer/negative_byte_sequence.mligo", line 1, characters 8-21:
  1 | let a = - (**) 0x2222
Negative byte sequence.
Hint: Remove the leading minus sign.
 |} ];

run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/negative_byte_sequence.religo" ; "main" ] ;
  [%expect {|
File "../../test/lexer/negative_byte_sequence.religo", line 1, characters 8-21:
  1 | let a = - /**/ 0x2222;
Negative byte sequence.
Hint: Remove the leading minus sign.
 |} ];


 run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/reserved_name.ligo" ; "main" ] ;
  [%expect {|
ligo: : Lexical error in file "reserved_name.ligo", line 1, characters 4-13:
      Reserved name: "arguments".
      Hint: Change the name.
       {}

 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/introduction
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/reserved_name.religo" ; "main" ] ;
  [%expect {|
File "../../test/lexer/reserved_name.religo", line 1, characters 4-7:
  1 | let end = 1;
Reserved name: "end".
Hint: Change the name.
 |} ];

run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/reserved_name.mligo" ; "main" ] ;
  [%expect {|
File "../../test/lexer/reserved_name.mligo", line 1, characters 4-10:
  1 | let object = 1;
Reserved name: "object".
Hint: Change the name.
 |} ];
  *)

run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/unexpected_character.ligo" ] ;
  [%expect {|
File "../../test/lexer/unexpected_character.ligo", line 1, characters 18-19:
  1 | const x: string = ���;
Unexpected character '\239'.
 |} ];

 run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/unexpected_character.mligo" ] ;
  [%expect {|
File "../../test/lexer/unexpected_character.mligo", line 1, characters 8-9:
  1 | let x = ���;
Unexpected character '\239'.
 |} ];

 run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/unexpected_character.religo" ] ;
  [%expect {|
File "../../test/lexer/unexpected_character.religo", line 1, characters 8-9:
  1 | let x = ���;
Unexpected character '\239'.
 |} ];

 run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/unterminated_comment.mligo" ] ;
  [%expect {|
File "../../test/lexer/unterminated_comment.mligo", line 1, characters 0-2:
  1 | (* not closed
File "../../test/lexer/unterminated_comment.mligo", line 1, characters 0-2:
Unterminated comment.
Hint: Close with "*)".
 |} ];

run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/invalid_symbol.ligo" ] ;
  [%expect {|
File "../../test/lexer/invalid_symbol.ligo", line 1, characters 17-20:
  1 | const b: int = 1 ... 10;
Invalid symbol: "...".
Hint: Check the LIGO syntax you use.
 |} ];

run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/invalid_symbol.mligo" ] ;
  [%expect {|
File "../../test/lexer/invalid_symbol.mligo", line 1, characters 10-13:
  1 | let b = 1 ... 10;
Invalid symbol: "...".
Hint: Check the LIGO syntax you use.
 |} ];

run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/invalid_symbol.religo" ] ;
  [%expect {|
File "../../test/lexer/invalid_symbol.religo", line 1, characters 10-11:
  1 | let b = 1 # 10;
Invalid symbol: "#".
Hint: Check the LIGO syntax you use.
 |} ];

 run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/missing_break.ligo" ] ;
  [%expect {|
File "../../test/lexer/missing_break.ligo", line 1, character 18:
  1 | const a: int = 300zennies;
Missing break.
Hint: Insert some space.
 |} ];

 run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/missing_break.mligo" ] ;
  [%expect {|
File "../../test/lexer/missing_break.mligo", line 1, character 11:
  1 | let a = 300zennies;
Missing break.
Hint: Insert some space.
 |} ];

 run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/missing_break.religo" ] ;
  [%expect {|
File "../../test/lexer/missing_break.religo", line 1, character 11:
  1 | let a = 300zennies;
Missing break.
Hint: Insert some space.
 |} ];

run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/invalid_character_in_string.ligo" ] ;
  [%expect {|
File "../../test/lexer/invalid_character_in_string.ligo", line 1, characters 19-20:
  1 | const z: string = "	";
Invalid character in string.
Hint: Remove or replace the character.
 |} ];

run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/invalid_character_in_string.mligo" ] ;
  [%expect {|
File "../../test/lexer/invalid_character_in_string.mligo", line 1, characters 9-10:
  1 | let z = "	";
Invalid character in string.
Hint: Remove or replace the character.
 |} ];

run_ligo_bad [ "compile" ; "contract" ; "../../test/lexer/invalid_character_in_string.religo" ] ;
  [%expect {|
File "../../test/lexer/invalid_character_in_string.religo", line 1, characters 9-10:
  1 | let z = "	";
Invalid character in string.
Hint: Remove or replace the character.
 |} ]
