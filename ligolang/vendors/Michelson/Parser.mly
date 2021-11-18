%{
(* START HEADER *)

    (*open AST*)

(* END HEADER *)
%}

(* Entry points *)

%start program
%type <unit> program (* AST.t *)

%%

(* RULES *)

(* Compounded constructs *)

paren(X): "(" X ")" { (*$1,$2,$3*) }
brace(X): "{" X "}" { (*$1,$2,$3*) }

(* Non-empty separated sequence of items *)

nsepseq(X,Sep):
  X                    {                        (*$1, []*) }
| X Sep nsepseq(X,Sep) { (*let h,t = $3 in $1, ($2,h)::t*) }

(* Possibly empty separated sequence of items *)

seq(X):
  nsepseq(X,";") { (*$1*) }

(* Main *)

program:
  "storage"   type__ ";"
  "parameter" type__ ";"
  "code" instruction ";"
  EOF {}
| "parameter" type__ ";"
  "storage"   type__ ";"
  "code" instruction ";"
  EOF {}

type__:
  constant_type_constructor

| "pair"     option(Tannot) field_type field_type
| "or"       option(Tannot) variant_type variant_type
| "option"   option(Tannot) subtype

| "list"     option(Tannot) subtype
| "set"      option(Tannot) comparable_type
| "contract" option(Tannot) subtype
| "lambda"   option(Tannot) subtype subtype
| "map"      option(Tannot) comparable_type subtype
| "big_map"  option(Tannot) comparable_type subtype

| paren(type__)
| paren(constant_type_constructor Tannot {}) {}

field_type:
  constant_type_constructor

| "pair"     option(Fannot) field_type field_type
| "or"       option(Fannot) variant_type variant_type
| "option"   option(Fannot) subtype

| "list"     option(Fannot) subtype
| "set"      option(Fannot) comparable_type
| "contract" option(Fannot) subtype
| "lambda"   option(Fannot) subtype subtype
| "map"      option(Fannot) comparable_type subtype
| "big_map"  option(Fannot) comparable_type subtype

| paren(field_type) {}
| paren(constant_type_constructor Fannot {}) {}

%inline variant_type: field_type {}

subtype:
  constant_type_constructor

| "pair"     field_type field_type
| "or"       variant_type variant_type
| "option"   subtype

| "list"     subtype
| "set"      comparable_type
| "contract" subtype
| "lambda"   subtype subtype
| "map"      comparable_type subtype
| "big_map"  comparable_type subtype

| paren(type__) {}

comparable_type:
  "int"
| "nat"
| "string"
| "bytes"
| "mutez"
| "bool"
| "key_hash"
| "timestamp" {}

constant_type_constructor:
  comparable_type
| "key"
| "unit"
| "signature"
| "operation"
| "address" {}

%inline domain: type__ {}
%inline range: type__ {}

data:
  "<string>"
| "<bytes>"
| "<int>"
| brace(seq("Elt" data data {})) (* Empty? *)
| "False"
| "Left" data
| "None"
| "Pair" data data
| "Right" data
| "Some" data
| "True"
| "Unit" {}

block:
  brace(seq(instruction)) {} (* Empty? *)

instruction:
  ABS
| ADD
| ADDRESS
| AMOUNT
| AND
| APPLY
| BALANCE
| BLAKE2B
| CHAIN_ID
| CAST
| CHECK_SIGNATURE
| COMPARE
| CONCAT
| CONS
| CONTRACT type__
| CREATE_CONTRACT block
| DIG
| DIP
| DROP
| DUG
| DUP
| EDIV
| EMPTY_BIG_MAP comparable_type type__
| EMPTY_MAP comparable_type type__
| EMPTY_SET comparable_type
| EQ
| EXEC
| FAILWITH data
| GE
| GET
| GT
| HASH_KEY
| IF block block
| IF_CONS block block
| IF_LEFT block block
| IF_NONE block block
| IF_RIGHT block block
| IMPLICIT_ACCOUNT
| INT
| ISNAT
| ITER block
| LAMBDA domain range block
| LE
| LEFT type__
| LOOP block
| LOOP_LEFT block
| LSL
| LSR
| LT
| MAP block
| MEM
| MUL
| NEG
| NEQ
| NIL type__
| NONE type__
| NOT
| NOW
| OR
| PACK
| PUSH type__ data
| RENAME
| RIGHT type__
| SELF
| SENDER
| SET_DELEGATE
| SHA256
| SHA512
| SIZE
| SLICE
| SOME
| SOURCE
| SUB
| SWAP
| TRANSFER_TOKENS
| UNIT
| UNPACK
| UPDATE
| XOR
| macro
| block {}

macro:
  ASSERT
| ASSERT_CMPEQ
| ASSERT_CMPGE
| ASSERT_CMPGT
| ASSERT_CMPLE
| ASSERT_CMPLT
| ASSERT_CMPNEQ
| ASSERT_EQ
| ASSERT_GE
| ASSERT_GT
| ASSERT_LE
| ASSERT_LEFT
| ASSERT_LT
| ASSERT_NEQ
| ASSERT_NONE
| ASSERT_RIGHT
| ASSERT_SOME
| CMPEQ
| CMPGE
| CMPGT
| CMPLE
| CMPLT
| CMPNEQ
| FAIL
| IFCMPEQ
| IFCMPGE
| IFCMPGT
| IFCMPLE
| IFCMPLT
| IFCMPNEQ
| IFEQ
| IFGE
| IFGT
| IFLE
| IFLT
| IFNEQ
| IF_NONE
| IF_SOME

(* Non-constant macros *)

| PAIR
| UNPAIR
| CADR
| SET_CADR
| MAP_CADR {}
