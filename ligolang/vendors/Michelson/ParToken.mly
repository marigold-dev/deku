%{
%}

(* Tokens mirroring those defined in module Token *)

  (* Literals *)

%token <Token.lexeme Region.reg>           String "<string>"
%token <(Token.lexeme * Hex.t) Region.reg> Bytes  "<bytes>"
%token <(Token.lexeme * Z.t) Region.reg>   Int    "<int>"

  (* Symbols *)

%token <Region.t> SEMI    ";"
%token <Region.t> LPAREN  "("
%token <Region.t> RPAREN  ")"
%token <Region.t> LBRACE  "{"
%token <Region.t> RBRACE  "}"

  (* Annotations *)

%token <string Region.reg> Tannot   (* Type  annotations *)
%token <string Region.reg> Vannot   (* Value annotations *)
%token <string Region.reg> Fannot   (* Field annotations *)

  (* Keywords *)

%token <Region.t> K_parameter "parameter"
%token <Region.t> K_storage   "storage"
%token <Region.t> K_code      "code"

  (* Types *)

%token <Region.t> T_address   "address"
%token <Region.t> T_big_map   "big_map"
%token <Region.t> T_bool      "bool"
%token <Region.t> T_bytes     "bytes"
%token <Region.t> T_contract  "contract"
%token <Region.t> T_int       "int"
%token <Region.t> T_key       "key"
%token <Region.t> T_key_hash  "key_hash"
%token <Region.t> T_lambda    "lambda"
%token <Region.t> T_list      "list"
%token <Region.t> T_map       "map"
%token <Region.t> T_mutez     "mutez"
%token <Region.t> T_nat       "nat"
%token <Region.t> T_operation "operation"
%token <Region.t> T_option    "option"
%token <Region.t> T_or        "or"
%token <Region.t> T_pair      "pair"
%token <Region.t> T_set       "set"
%token <Region.t> T_signature "signature"
%token <Region.t> T_string    "string"
%token <Region.t> T_timestamp "timestamp"
%token <Region.t> T_unit      "unit"

  (* Data *)

%token <Region.t> D_Elt   "Elt"
%token <Region.t> D_False "False"
%token <Region.t> D_Left  "Left"
%token <Region.t> D_None  "None"
%token <Region.t> D_Pair  "Pair"
%token <Region.t> D_Right "Right"
%token <Region.t> D_Some  "Some"
%token <Region.t> D_True  "True"
%token <Region.t> D_Unit  "Unit"

  (* Instructions *)

%token <Region.t> ABS
%token <Region.t> ADD
%token <Region.t> ADDRESS
%token <Region.t> AMOUNT
%token <Region.t> AND
%token <Region.t> APPLY
%token <Region.t> BALANCE
%token <Region.t> BLAKE2B
%token <Region.t> CHAIN_ID
%token <Region.t> CAST
%token <Region.t> CHECK_SIGNATURE
%token <Region.t> COMPARE
%token <Region.t> CONCAT
%token <Region.t> CONS
%token <Region.t> CONTRACT
%token <Region.t> CREATE_CONTRACT
%token <Region.t> DIG
%token <Region.t> DIP
%token <Region.t> DROP
%token <Region.t> DUG
%token <Region.t> DUP
%token <Region.t> EDIV
%token <Region.t> EMPTY_BIG_MAP
%token <Region.t> EMPTY_MAP
%token <Region.t> EMPTY_SET
%token <Region.t> EQ
%token <Region.t> EXEC
%token <Region.t> FAILWITH
%token <Region.t> GE
%token <Region.t> GET
%token <Region.t> GT
%token <Region.t> HASH_KEY
%token <Region.t> IF
%token <Region.t> IF_CONS
%token <Region.t> IF_LEFT
%token <Region.t> IF_RIGHT
%token <Region.t> IMPLICIT_ACCOUNT
%token <Region.t> INT
%token <Region.t> ISNAT
%token <Region.t> ITER
%token <Region.t> LAMBDA
%token <Region.t> LE
%token <Region.t> LEFT
%token <Region.t> LOOP
%token <Region.t> LOOP_LEFT
%token <Region.t> LSL
%token <Region.t> LSR
%token <Region.t> LT
%token <Region.t> MAP
%token <Region.t> MEM
%token <Region.t> MUL
%token <Region.t> NEG
%token <Region.t> NEQ
%token <Region.t> NIL
%token <Region.t> NONE
%token <Region.t> NOT
%token <Region.t> NOW
%token <Region.t> OR
%token <Region.t> PACK
%token <Region.t> PUSH
%token <Region.t> RENAME
%token <Region.t> RIGHT
%token <Region.t> SELF
%token <Region.t> SENDER
%token <Region.t> SET_DELEGATE
%token <Region.t> SHA256
%token <Region.t> SHA512
%token <Region.t> SIZE
%token <Region.t> SLICE
%token <Region.t> SOME
%token <Region.t> SOURCE
%token <Region.t> SUB
%token <Region.t> SWAP
%token <Region.t> TRANSFER_TOKENS
%token <Region.t> UNIT
%token <Region.t> UNPACK
%token <Region.t> UPDATE
%token <Region.t> XOR

(* Macros *)

%token <Region.t> ASSERT
%token <Region.t> ASSERT_CMPEQ
%token <Region.t> ASSERT_CMPGE
%token <Region.t> ASSERT_CMPGT
%token <Region.t> ASSERT_CMPLE
%token <Region.t> ASSERT_CMPLT
%token <Region.t> ASSERT_CMPNEQ
%token <Region.t> ASSERT_EQ
%token <Region.t> ASSERT_GE
%token <Region.t> ASSERT_GT
%token <Region.t> ASSERT_LE
%token <Region.t> ASSERT_LEFT
%token <Region.t> ASSERT_LT
%token <Region.t> ASSERT_NEQ
%token <Region.t> ASSERT_NONE
%token <Region.t> ASSERT_RIGHT
%token <Region.t> ASSERT_SOME
%token <Region.t> CMPEQ
%token <Region.t> CMPGE
%token <Region.t> CMPGT
%token <Region.t> CMPLE
%token <Region.t> CMPLT
%token <Region.t> CMPNEQ
%token <Region.t> FAIL
%token <Region.t> IFCMPEQ
%token <Region.t> IFCMPGE
%token <Region.t> IFCMPGT
%token <Region.t> IFCMPLE
%token <Region.t> IFCMPLT
%token <Region.t> IFCMPNEQ
%token <Region.t> IFEQ
%token <Region.t> IFGE
%token <Region.t> IFGT
%token <Region.t> IFLE
%token <Region.t> IFLT
%token <Region.t> IFNEQ
%token <Region.t> IF_NONE
%token <Region.t> IF_SOME

  (* Non-constant macros *)

%token <Pair.tree Region.reg> PAIR
%token <Pair.tree Region.reg> UNPAIR
%token <Pair.tree Region.reg> CADR
%token <Pair.tree Region.reg> SET_CADR
%token <Pair.tree Region.reg> MAP_CADR

  (* Virtual tokens *)

%token <Region.t> EOF

%%
