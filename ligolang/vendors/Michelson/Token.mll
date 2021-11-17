(* Lexer specification for Michelson lexemes, to be processed by
   [ocamllex] *)

{
(* START HEADER *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Markup = LexerLib.Markup

(* Utility modules *)

module SMap = Map.Make (String)

(* Shorhands *)

let sprintf = Printf.sprintf

(* TOKENS *)

type lexeme = string

let max_annot_length = 255 (* Maximum lengths of annotations *)

(* Create associative lists from strings *)

let add map (key,value) = SMap.add key value map

let mk_map mk_key list =
  let apply map value = add map (mk_key value, value)
  in List.fold_left apply SMap.empty list

(* ANNOTATIONS *)

type annotation =
  Tannot of string Region.reg  (* Type  annotations *)
| Vannot of string Region.reg  (* Value annotations *)
| Fannot of string Region.reg  (* Field annotations *)

let annot_to_lexeme = function
  Tannot a | Vannot a | Fannot a -> a.Region.value

let proj_annot = function
  Tannot Region.{region; value} ->
    region, sprintf "Tannot %S" value
| Vannot Region.{region; value} ->
    region, sprintf "Vannot %S" value
| Fannot Region.{region; value} ->
    region, sprintf "Fannot %S" value

let annot_to_string ~offsets mode annot =
  let region, val_str = proj_annot annot in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str val_str

(* KEYWORDS *)

type keyword =
  K_parameter of Region.t  (* "parameter" *)
  | K_storage   of Region.t  (* "storage"   *)
  | K_code      of Region.t  (* "code"      *)

let keyword_to_lexeme = function
   K_parameter _ -> "parameter"
| K_storage _   -> "storage"
| K_code _      -> "code"

let proj_keyword = function
  K_parameter region -> region, "K_parameter"
| K_storage region   -> region, "K_storage"
| K_code region      -> region, "K_code"

let keyword_to_string ~offsets mode kwd =
  let region, val_str = proj_keyword kwd in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str val_str

let kwd_list = [
  (fun reg -> K_storage reg);
  (fun reg -> K_parameter reg);
  (fun reg -> K_code reg)
  ]

let kwd_map =
  mk_map (fun f -> keyword_to_lexeme (f Region.ghost)) kwd_list

(* DATA *)

type data =
  D_Elt   of Region.t  (* "Elt"   *)
| D_False of Region.t  (* "False" *)
| D_Left  of Region.t  (* "Left"  *)
| D_None  of Region.t  (* "None"  *)
| D_Pair  of Region.t  (* "Pair"  *)
| D_Right of Region.t  (* "Right" *)
| D_Some  of Region.t  (* "Some"  *)
| D_True  of Region.t  (* "True"  *)
| D_Unit  of Region.t  (* "Unit"  *)

let data_list = [
  (fun reg -> D_False reg);
  (fun reg -> D_Elt   reg);
  (fun reg -> D_Left  reg);
  (fun reg -> D_None  reg);
  (fun reg -> D_Pair  reg);
  (fun reg -> D_Right reg);
  (fun reg -> D_Some  reg);
  (fun reg -> D_True  reg);
  (fun reg -> D_Unit  reg)
]

let data_to_lexeme = function
  D_False _ -> "False"
| D_Elt   _ -> "Elt"
| D_Left  _ -> "Left"
| D_None  _ -> "None"
| D_Pair  _ -> "Pair"
| D_Right _ -> "Right"
| D_Some  _ -> "Some"
| D_True  _ -> "True"
| D_Unit  _ -> "Unit"

let proj_data = function
  D_False region -> region, "D_False"
| D_Elt   region -> region, "D_Elt"
| D_Left  region -> region, "D_Left"
| D_None  region -> region, "D_None"
| D_Pair  region -> region, "D_Pair"
| D_Right region -> region, "D_Right"
| D_Some  region -> region, "D_Some"
| D_True  region -> region, "D_True"
| D_Unit  region -> region, "D_Unit"

let data_to_string ~offsets mode data =
  let region, val_str = proj_data data in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str val_str

let data_map =
  mk_map (fun f -> data_to_lexeme (f Region.ghost)) data_list

(* INSTRUCTIONS *)

type instruction =
  ABS              of Region.t
| ADD              of Region.t
| ADDRESS          of Region.t
| AMOUNT           of Region.t
| AND              of Region.t
| APPLY            of Region.t
| BALANCE          of Region.t
| BLAKE2B          of Region.t
| CHAIN_ID         of Region.t
| CAST             of Region.t
| CHECK_SIGNATURE  of Region.t
| COMPARE          of Region.t
| CONCAT           of Region.t
| CONS             of Region.t
| CONTRACT         of Region.t
| CREATE_CONTRACT  of Region.t
| DIG              of Region.t
| DIP              of Region.t
| DROP             of Region.t
| DUG              of Region.t
| DUP              of Region.t
| EDIV             of Region.t
| EMPTY_BIG_MAP    of Region.t
| EMPTY_MAP        of Region.t
| EMPTY_SET        of Region.t
| EQ               of Region.t
| EXEC             of Region.t
| FAILWITH         of Region.t
| GE               of Region.t
| GET              of Region.t
| GT               of Region.t
| HASH_KEY         of Region.t
| IF               of Region.t
| IF_CONS          of Region.t
| IF_LEFT          of Region.t
| IF_RIGHT         of Region.t
| IMPLICIT_ACCOUNT of Region.t
| INT              of Region.t
| ISNAT            of Region.t
| ITER             of Region.t
| LAMBDA           of Region.t
| LE               of Region.t
| LEFT             of Region.t
| LOOP             of Region.t
| LOOP_LEFT        of Region.t
| LSL              of Region.t
| LSR              of Region.t
| LT               of Region.t
| MAP              of Region.t
| MEM              of Region.t
| MUL              of Region.t
| NEG              of Region.t
| NEQ              of Region.t
| NIL              of Region.t
| NONE             of Region.t
| NOT              of Region.t
| NOW              of Region.t
| OR               of Region.t
| PACK             of Region.t
| PUSH             of Region.t
| RENAME           of Region.t
| RIGHT            of Region.t
| SELF             of Region.t
| SENDER           of Region.t
| SET_DELEGATE     of Region.t
| SHA256           of Region.t
| SHA512           of Region.t
| SIZE             of Region.t
| SLICE            of Region.t
| SOME             of Region.t
| SOURCE           of Region.t
| SUB              of Region.t
| SWAP             of Region.t
| TRANSFER_TOKENS  of Region.t
| UNIT             of Region.t
| UNPACK           of Region.t
| UPDATE           of Region.t
| XOR              of Region.t

let instr_list = [
  (fun reg -> ABS              reg);
  (fun reg -> ADD              reg);
  (fun reg -> ADDRESS          reg);
  (fun reg -> AMOUNT           reg);
  (fun reg -> AND              reg);
  (fun reg -> APPLY            reg);
  (fun reg -> BALANCE          reg);
  (fun reg -> BLAKE2B          reg);
  (fun reg -> CHAIN_ID         reg);
  (fun reg -> CAST             reg);
  (fun reg -> CHECK_SIGNATURE  reg);
  (fun reg -> COMPARE          reg);
  (fun reg -> CONCAT           reg);
  (fun reg -> CONS             reg);
  (fun reg -> CONTRACT         reg);
  (fun reg -> CREATE_CONTRACT  reg);
  (fun reg -> DIG              reg);
  (fun reg -> DIP              reg);
  (fun reg -> DROP             reg);
  (fun reg -> DUG              reg);
  (fun reg -> DUP              reg);
  (fun reg -> EDIV             reg);
  (fun reg -> EMPTY_BIG_MAP    reg);
  (fun reg -> EMPTY_MAP        reg);
  (fun reg -> EMPTY_SET        reg);
  (fun reg -> EQ               reg);
  (fun reg -> EXEC             reg);
  (fun reg -> FAILWITH         reg);
  (fun reg -> GE               reg);
  (fun reg -> GET              reg);
  (fun reg -> GT               reg);
  (fun reg -> HASH_KEY         reg);
  (fun reg -> IF               reg);
  (fun reg -> IF_CONS          reg);
  (fun reg -> IF_LEFT          reg);
  (fun reg -> IF_RIGHT         reg);
  (fun reg -> IMPLICIT_ACCOUNT reg);
  (fun reg -> INT              reg);
  (fun reg -> ISNAT            reg);
  (fun reg -> ITER             reg);
  (fun reg -> LAMBDA           reg);
  (fun reg -> LE               reg);
  (fun reg -> LEFT             reg);
  (fun reg -> LOOP             reg);
  (fun reg -> LOOP_LEFT        reg);
  (fun reg -> LSL              reg);
  (fun reg -> LSR              reg);
  (fun reg -> LT               reg);
  (fun reg -> MAP              reg);
  (fun reg -> MEM              reg);
  (fun reg -> MUL              reg);
  (fun reg -> NEG              reg);
  (fun reg -> NEQ              reg);
  (fun reg -> NIL              reg);
  (fun reg -> NONE             reg);
  (fun reg -> NOT              reg);
  (fun reg -> NOW              reg);
  (fun reg -> OR               reg);
  (fun reg -> PACK             reg);
  (fun reg -> PUSH             reg);
  (fun reg -> RENAME           reg);
  (fun reg -> RIGHT            reg);
  (fun reg -> SELF             reg);
  (fun reg -> SENDER           reg);
  (fun reg -> SET_DELEGATE     reg);
  (fun reg -> SHA256           reg);
  (fun reg -> SHA512           reg);
  (fun reg -> SIZE             reg);
  (fun reg -> SLICE            reg);
  (fun reg -> SOME             reg);
  (fun reg -> SOURCE           reg);
  (fun reg -> SUB              reg);
  (fun reg -> SWAP             reg);
  (fun reg -> TRANSFER_TOKENS  reg);
  (fun reg -> UNIT             reg);
  (fun reg -> UNPACK           reg);
  (fun reg -> UPDATE           reg);
  (fun reg -> XOR              reg)
]

let proj_instr = function
  ABS              region -> region, "ABS"
| ADD              region -> region, "ADD"
| ADDRESS          region -> region, "ADDRESS"
| AMOUNT           region -> region, "AMOUNT"
| AND              region -> region, "AND"
| APPLY            region -> region, "APPLY"
| BALANCE          region -> region, "BALANCE"
| BLAKE2B          region -> region, "BLAKE2B"
| CHAIN_ID         region -> region, "CHAIN_ID"
| CAST             region -> region, "CAST"
| CHECK_SIGNATURE  region -> region, "CHECK_SIGNATURE"
| COMPARE          region -> region, "COMPARE"
| CONCAT           region -> region, "CONCAT"
| CONS             region -> region, "CONS"
| CONTRACT         region -> region, "CONTRACT"
| CREATE_CONTRACT  region -> region, "CREATE_CONTRACT"
| DIG              region -> region, "DIG"
| DIP              region -> region, "DIP"
| DROP             region -> region, "DROP"
| DUG              region -> region, "DUG"
| DUP              region -> region, "DUP"
| EDIV             region -> region, "EDIV"
| EMPTY_BIG_MAP    region -> region, "EMPTY_BIG_MAP"
| EMPTY_MAP        region -> region, "EMPTY_MAP"
| EMPTY_SET        region -> region, "EMPTY_SET"
| EQ               region -> region, "EQ"
| EXEC             region -> region, "EXEC"
| FAILWITH         region -> region, "FAILWITH"
| GE               region -> region, "GE"
| GET              region -> region, "GET"
| GT               region -> region, "GT"
| HASH_KEY         region -> region, "HASH_KEY"
| IF               region -> region, "IF"
| IF_CONS          region -> region, "IF_CONS"
| IF_LEFT          region -> region, "IF_LEFT"
| IF_RIGHT         region -> region, "IF_RIGHT"
| IMPLICIT_ACCOUNT region -> region, "IMPLICIT_ACCOUNT"
| INT              region -> region, "INT"
| ISNAT            region -> region, "ISNAT"
| ITER             region -> region, "ITER"
| LAMBDA           region -> region, "LAMBDA"
| LE               region -> region, "LE"
| LEFT             region -> region, "LEFT"
| LOOP             region -> region, "LOOP"
| LOOP_LEFT        region -> region, "LOOP_LEFT"
| LSL              region -> region, "LSL"
| LSR              region -> region, "LSR"
| LT               region -> region, "LT"
| MAP              region -> region, "MAP"
| MEM              region -> region, "MEM"
| MUL              region -> region, "MUL"
| NEG              region -> region, "NEG"
| NEQ              region -> region, "NEQ"
| NIL              region -> region, "NIL"
| NONE             region -> region, "NONE"
| NOT              region -> region, "NOT"
| NOW              region -> region, "NOW"
| OR               region -> region, "OR"
| PACK             region -> region, "PACK"
| PUSH             region -> region, "PUSH"
| RENAME           region -> region, "RENAME"
| RIGHT            region -> region, "RIGHT"
| SELF             region -> region, "SELF"
| SENDER           region -> region, "SENDER"
| SET_DELEGATE     region -> region, "SET_DELEGATE"
| SHA256           region -> region, "SHA256"
| SHA512           region -> region, "SHA512"
| SIZE             region -> region, "SIZE"
| SLICE            region -> region, "SLICE"
| SOME             region -> region, "SOME"
| SOURCE           region -> region, "SOURCE"
| SUB              region -> region, "SUB"
| SWAP             region -> region, "SWAP"
| TRANSFER_TOKENS  region -> region, "TRANSFER_TOKENS"
| UNIT             region -> region, "UNIT"
| UNPACK           region -> region, "UNPACK"
| UPDATE           region -> region, "UPDATE"
| XOR              region -> region, "XOR"

let instr_to_lexeme instr = proj_instr instr |> snd

let instr_to_string ~offsets mode instr =
  let region, val_str = proj_instr instr in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str val_str

let instr_map =
  mk_map (fun f -> instr_to_lexeme (f Region.ghost)) instr_list

(* MACROS *)

type macro =
  (* Constant macros *)

  ASSERT        of Region.t
| ASSERT_CMPEQ  of Region.t
| ASSERT_CMPGE  of Region.t
| ASSERT_CMPGT  of Region.t
| ASSERT_CMPLE  of Region.t
| ASSERT_CMPLT  of Region.t
| ASSERT_CMPNEQ of Region.t
| ASSERT_EQ     of Region.t
| ASSERT_GE     of Region.t
| ASSERT_GT     of Region.t
| ASSERT_LE     of Region.t
| ASSERT_LEFT   of Region.t
| ASSERT_LT     of Region.t
| ASSERT_NEQ    of Region.t
| ASSERT_NONE   of Region.t
| ASSERT_RIGHT  of Region.t
| ASSERT_SOME   of Region.t
| CMPEQ         of Region.t
| CMPGE         of Region.t
| CMPGT         of Region.t
| CMPLE         of Region.t
| CMPLT         of Region.t
| CMPNEQ        of Region.t
| FAIL          of Region.t
| IFCMPEQ       of Region.t
| IFCMPGE       of Region.t
| IFCMPGT       of Region.t
| IFCMPLE       of Region.t
| IFCMPLT       of Region.t
| IFCMPNEQ      of Region.t
| IFEQ          of Region.t
| IFGE          of Region.t
| IFGT          of Region.t
| IFLE          of Region.t
| IFLT          of Region.t
| IFNEQ         of Region.t
| IF_NONE       of Region.t
| IF_SOME       of Region.t

(* Non-constant macros *)

| PAIR     of Pair.tree Region.reg
| UNPAIR   of Pair.tree Region.reg
| CADR     of Pair.path Region.reg
| SET_CADR of Pair.path Region.reg
| MAP_CADR of Pair.path Region.reg

let macro_list = [
  (fun reg -> ASSERT        reg);
  (fun reg -> ASSERT_CMPEQ  reg);
  (fun reg -> ASSERT_CMPGE  reg);
  (fun reg -> ASSERT_CMPGT  reg);
  (fun reg -> ASSERT_CMPLE  reg);
  (fun reg -> ASSERT_CMPLT  reg);
  (fun reg -> ASSERT_CMPNEQ reg);
  (fun reg -> ASSERT_EQ     reg);
  (fun reg -> ASSERT_GE     reg);
  (fun reg -> ASSERT_GT     reg);
  (fun reg -> ASSERT_LE     reg);
  (fun reg -> ASSERT_LEFT   reg);
  (fun reg -> ASSERT_LT     reg);
  (fun reg -> ASSERT_NEQ    reg);
  (fun reg -> ASSERT_NONE   reg);
  (fun reg -> ASSERT_RIGHT  reg);
  (fun reg -> ASSERT_SOME   reg);
  (fun reg -> CMPEQ         reg);
  (fun reg -> CMPGE         reg);
  (fun reg -> CMPGT         reg);
  (fun reg -> CMPLE         reg);
  (fun reg -> CMPLT         reg);
  (fun reg -> CMPNEQ        reg);
  (fun reg -> FAIL          reg);
  (fun reg -> IFCMPEQ       reg);
  (fun reg -> IFCMPGE       reg);
  (fun reg -> IFCMPGT       reg);
  (fun reg -> IFCMPLE       reg);
  (fun reg -> IFCMPLT       reg);
  (fun reg -> IFCMPNEQ      reg);
  (fun reg -> IFEQ          reg);
  (fun reg -> IFGE          reg);
  (fun reg -> IFGT          reg);
  (fun reg -> IFLE          reg);
  (fun reg -> IFLT          reg);
  (fun reg -> IFNEQ         reg);
  (fun reg -> IF_NONE       reg);
  (fun reg -> IF_SOME       reg)
]

let proj_macro = function
  (* Constant macros *)
  ASSERT        region -> region, "ASSERT"
| ASSERT_CMPEQ  region -> region, "ASSERT_CMPEQ"
| ASSERT_CMPGE  region -> region, "ASSERT_CMPGE"
| ASSERT_CMPGT  region -> region, "ASSERT_CMPGT"
| ASSERT_CMPLE  region -> region, "ASSERT_CMPLE"
| ASSERT_CMPLT  region -> region, "ASSERT_CMPLT"
| ASSERT_CMPNEQ region -> region, "ASSERT_CMPNEQ"
| ASSERT_EQ     region -> region, "ASSERT_EQ"
| ASSERT_GE     region -> region, "ASSERT_GE"
| ASSERT_GT     region -> region, "ASSERT_GT"
| ASSERT_LE     region -> region, "ASSERT_LE"
| ASSERT_LEFT   region -> region, "ASSERT_LEFT"
| ASSERT_LT     region -> region, "ASSERT_LT"
| ASSERT_NEQ    region -> region, "ASSERT_NEQ"
| ASSERT_NONE   region -> region, "ASSERT_NONE"
| ASSERT_RIGHT  region -> region, "ASSERT_RIGHT"
| ASSERT_SOME   region -> region, "ASSERT_SOME"
| CMPEQ         region -> region, "CMPEQ"
| CMPGE         region -> region, "CMPGE"
| CMPGT         region -> region, "CMPGT"
| CMPLE         region -> region, "CMPLE"
| CMPLT         region -> region, "CMPLT"
| CMPNEQ        region -> region, "CMPNEQ"
| FAIL          region -> region, "FAIL"
| IFCMPEQ       region -> region, "IFCMPEQ"
| IFCMPGE       region -> region, "IFCMPGE"
| IFCMPGT       region -> region, "IFCMPGT"
| IFCMPLE       region -> region, "IFCMPLE"
| IFCMPLT       region -> region, "IFCMPLT"
| IFCMPNEQ      region -> region, "IFCMPNEQ"
| IFEQ          region -> region, "IFEQ"
| IFGE          region -> region, "IFGE"
| IFGT          region -> region, "IFGT"
| IFLE          region -> region, "IFLE"
| IFLT          region -> region, "IFLT"
| IFNEQ         region -> region, "IFNEQ"
| IF_NONE       region -> region, "IF_NONE"
| IF_SOME       region -> region, "IF_SOME"

(* Non-constant macros *)

| PAIR Region.{region; value=tree} ->
    region, sprintf "%sR" Pair.(encode tree |> drop)
| UNPAIR Region.{region; value=tree} ->
    region, sprintf "UN%sR" Pair.(encode tree |> drop)
| CADR Region.{region; value} ->
    region, Pair.drop_path value
| SET_CADR Region.{region; value} ->
    region, sprintf "SET_%s" (Pair.drop_path value)
| MAP_CADR Region.{region; value} ->
    region, sprintf "MAP_%s" (Pair.drop_path value)

let macro_to_lexeme macro = proj_macro macro |> snd

let macro_to_string ~offsets mode macro =
  let region, val_str = proj_macro macro in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str val_str

let macro_map = mk_map (fun f -> macro_to_lexeme (f Region.ghost)) macro_list

(* TYPES *)

type m_type =
  T_address   of Region.t  (* "address"   *)
| T_big_map   of Region.t  (* "big_map"   *)
| T_bool      of Region.t  (* "bool"      *)
| T_bytes     of Region.t  (* "bytes"     *)
| T_chain_id  of Region.t  (* "chain_id"  *)
| T_contract  of Region.t  (* "contract"  *)
| T_int       of Region.t  (* "int"       *)
| T_key       of Region.t  (* "key"       *)
| T_key_hash  of Region.t  (* "key_hash"  *)
| T_lambda    of Region.t  (* "lambda"    *)
| T_list      of Region.t  (* "list"      *)
| T_map       of Region.t  (* "map"       *)
| T_mutez     of Region.t  (* "mutez"     *)
| T_nat       of Region.t  (* "nat"       *)
| T_operation of Region.t  (* "operation" *)
| T_option    of Region.t  (* "option"    *)
| T_or        of Region.t  (* "or"        *)
| T_pair      of Region.t  (* "pair"      *)
| T_set       of Region.t  (* "set"       *)
| T_signature of Region.t  (* "signature" *)
| T_string    of Region.t  (* "string"    *)
| T_timestamp of Region.t  (* "timestamp" *)
| T_unit      of Region.t  (* "unit"      *)

let type_list = [
  (fun reg -> T_address    reg);
  (fun reg -> T_big_map    reg);
  (fun reg -> T_bool       reg);
  (fun reg -> T_bytes      reg);
  (fun reg -> T_chain_id   reg);
  (fun reg -> T_contract   reg);
  (fun reg -> T_int        reg);
  (fun reg -> T_key        reg);
  (fun reg -> T_key_hash   reg);
  (fun reg -> T_lambda     reg);
  (fun reg -> T_list       reg);
  (fun reg -> T_map        reg);
  (fun reg -> T_mutez      reg);
  (fun reg -> T_nat        reg);
  (fun reg -> T_operation  reg);
  (fun reg -> T_option     reg);
  (fun reg -> T_or         reg);
  (fun reg -> T_pair       reg);
  (fun reg -> T_set        reg);
  (fun reg -> T_signature  reg);
  (fun reg -> T_string     reg);
  (fun reg -> T_timestamp  reg);
  (fun reg -> T_unit       reg)
]

let type_to_lexeme = function
  T_address   _ -> "address"
| T_big_map   _ -> "big_map"
| T_bool      _ -> "bool"
| T_bytes     _ -> "bytes"
| T_chain_id  _ -> "chain_id"
| T_contract  _ -> "contract"
| T_int       _ -> "int"
| T_key       _ -> "key"
| T_key_hash  _ -> "key_hash"
| T_lambda    _ -> "lambda"
| T_list      _ -> "list"
| T_map       _ -> "map"
| T_mutez     _ -> "mutez"
| T_nat       _ -> "nat"
| T_operation _ -> "operation"
| T_option    _ -> "option"
| T_or        _ -> "or"
| T_pair      _ -> "pair"
| T_set       _ -> "set"
| T_signature _ -> "signature"
| T_string    _ -> "string"
| T_timestamp _ -> "timestamp"
| T_unit      _ -> "unit"

let proj_type = function
  T_address   region -> region, "T_address"
| T_big_map   region -> region, "T_big_map"
| T_bool      region -> region, "T_bool"
| T_bytes     region -> region, "T_bytes"
| T_chain_id  region -> region, "T_chain_id"
| T_contract  region -> region, "T_contract"
| T_int       region -> region, "T_int"
| T_key       region -> region, "T_key"
| T_key_hash  region -> region, "T_key_hash"
| T_lambda    region -> region, "T_lambda"
| T_list      region -> region, "T_list"
| T_map       region -> region, "T_map"
| T_mutez     region -> region, "T_mutez"
| T_nat       region -> region, "T_nat"
| T_operation region -> region, "T_operation"
| T_option    region -> region, "T_option"
| T_or        region -> region, "T_or"
| T_pair      region -> region, "T_pair"
| T_set       region -> region, "T_set"
| T_signature region -> region, "T_signature"
| T_string    region -> region, "T_string"
| T_timestamp region -> region, "T_timestamp"
| T_unit      region -> region, "T_unit"

let type_to_string ~offsets mode type_ =
  let region, val_str = proj_type type_ in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str val_str

let type_map = mk_map (fun f -> type_to_lexeme (f Region.ghost)) type_list

(* LEXIS *)

type lexis = {
  kwd_map   : (Region.t -> keyword)     SMap.t;
  data_map  : (Region.t -> data)        SMap.t;
  instr_map : (Region.t -> instruction) SMap.t;
  macro_map : (Region.t -> macro)       SMap.t;
  type_map  : (Region.t -> m_type)      SMap.t
}

let lexicon = {kwd_map; data_map; instr_map; macro_map; type_map}

(* TOKENS *)

type t =
  String  of lexeme Region.reg
| Bytes   of (lexeme * Hex.t) Region.reg
| Int     of (lexeme * Z.t) Region.reg
| Keyword of keyword
| Data    of data
| Instr   of instruction
| Macro   of macro
| Type    of m_type
| Annot   of annotation
| SEMI    of Region.t
| LPAREN  of Region.t
| RPAREN  of Region.t
| LBRACE  of Region.t
| RBRACE  of Region.t
| EOF     of Region.t

type token = t

let to_lexeme = function
  String s  -> s.Region.value
| Bytes b   -> fst b.Region.value
| Int i     -> fst i.Region.value
| Keyword k -> keyword_to_lexeme k
| Data d    -> data_to_lexeme d
| Instr i   -> instr_to_lexeme i
| Macro i   -> macro_to_lexeme i
| Type t    -> type_to_lexeme t
| Annot a   -> annot_to_lexeme a
| SEMI _    -> ";"
| LPAREN _  -> "("
| RPAREN _  -> ")"
| LBRACE _  -> "{"
| RBRACE _  -> "}"
| EOF _     -> ""

let proj_token = function
  String Region.{region; value} ->
    region, sprintf "String %S" value
| Bytes Region.{region; value = s,b} ->
    region,
    sprintf "Bytes (%S, \"0x%s\")" s (Hex.show b)
| Int Region.{region; value=s,n} ->
    region, sprintf "Int (%S, %s)" s (Z.to_string n)
| Keyword k -> proj_keyword k
| Data d -> proj_data d
| Instr i -> proj_instr i
| Macro i -> proj_macro i
| Type t -> proj_type t
| Annot a -> proj_annot a

| SEMI region -> region, "SEMI"
| LPAREN region -> region, "LPAREN"
| RPAREN region -> region, "RPAREN"
| LBRACE region -> region, "LBRACE"
| RBRACE region -> region, "RBRACE"
| EOF region -> region, "EOF"

let to_string ~offsets mode token =
  let region, val_str = proj_token token in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str val_str

let to_region token = proj_token token |> fst

(* Errors in the recognition of an identifier *)

type ident_err =
  Valid_prefix       of Pair.index * Pair.tree
| Invalid_tree       of Pair.index * char * Pair.tree
| Truncated_encoding of Pair.index * Pair.child * Pair.tree
| Missing_break      of int
| Invalid_identifier

(* The function [mk_tree] refines the errors exported by the module
   [Pair]: when an invalid pair constructor is found,
   [Pair.Invalid_tree] is raised and handled here: if the end of the
   encoding was actually reached, this exception becomes the error
   [Truncated_encoding]. If not, we get an invalid tree. This enables
   a finer error reporting. *)

let mk_tree offset code region =
  match Pair.(lift code |> decode) with
    Error (Pair.Valid_prefix (index, tree)) ->
      Error (Valid_prefix (index + offset, tree))
  | Error (Pair.Invalid_tree (index, tree, child)) ->
      let error =
        if index = String.length code then
          Truncated_encoding (index + offset, child, tree)
        else Invalid_tree (index + offset, code.[index], tree)
      in Error error
  | Ok value ->
      let payload = Region.{region; value} in
      let macro = if offset = 0 then PAIR payload else UNPAIR payload
      in Ok (Macro macro)

let mk_macro f region code =
  Ok (Macro (f Region.{region; value = Pair.lift_path code}))

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

let pair      = ('P' ('P'|'A'|'I')+ as code) 'R'
let unpair    = "UN" pair
let cadr      = 'C' ('A'|'D')+ 'R' as code
let set_cadr  = "SET_" cadr
let map_cadr  = "MAP_" cadr

let uppercase = ['A'-'Z']
let lowercase = ['a'-'z']
let digit     = ['0'-'9']
let natural   = digit | digit (digit | '_')* digit
let instr     = uppercase (uppercase | '_' (uppercase | natural) | natural)*
let data      = uppercase lowercase+ as id
let kwd_type  = lowercase (lowercase | '_')* as id

(* Rules *)

rule scan_ident region lexicon = parse
  pair     eof { mk_tree 0 code region }
| unpair   eof { mk_tree 2 code region }
| cadr     eof { mk_macro (fun r -> CADR r) region code }
| set_cadr eof { mk_macro (fun r -> SET_CADR r) region code }
| map_cadr eof { mk_macro (fun r -> MAP_CADR r) region code }

| (instr as id) eof {
    match SMap.find_opt id lexicon.instr_map with
      Some mk_instr -> Ok (Instr (mk_instr region))
    | None -> match SMap.find_opt id lexicon.macro_map with
                Some mk_macro -> Ok (Macro (mk_macro region))
              | None -> Error Invalid_identifier }

| data eof {
    match SMap.find_opt id lexicon.data_map with
      Some mk_data -> Ok (Data (mk_data region))
    | None -> Error Invalid_identifier }

| kwd_type eof {
    match SMap.find_opt id lexicon.kwd_map with
      Some mk_kwd -> Ok (Keyword (mk_kwd region))
    | None -> match SMap.find_opt id lexicon.type_map with
                Some mk_type -> Ok (Type (mk_type region))
              | None -> Error Invalid_identifier }

| instr { let stop  = Lexing.lexeme_end_p lexbuf in
          let index = stop.Lexing.pos_cnum in
          match scan_ident region lexicon lexbuf with
            Ok _ -> Error (Missing_break index)
          | Error _ -> Error Invalid_identifier }

| _ { Error Invalid_identifier }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

(* Smart constructors (injections) *)

let mk_string lexeme region = String Region.{region; value=lexeme}

let mk_bytes lexeme region =
  let norm = Str.(global_replace (regexp "_") "" lexeme) in
  let value = lexeme, `Hex norm
  in Bytes Region.{region; value}

type int_err = Non_canonical_zero

let mk_int lexeme region =
  let z =
    Str.(global_replace (regexp "_") "" lexeme) |> Z.of_string
  in if   Z.equal z Z.zero && lexeme <> "0"
     then Error Non_canonical_zero
     else Ok (Int Region.{region; value = lexeme,z})

let eof region = EOF region

let mk_sym lexeme region =
  match lexeme with
    ";" -> SEMI   region
  | "(" -> LPAREN region
  | ")" -> RPAREN region
  | "{" -> LBRACE region
  | "}" -> RBRACE region
  | _   -> assert false (* See regexp "symbol" in Lexer.mll. *)

type annot_err = Annotation_length of int

let mk_annot lexeme region =
  if String.length lexeme - 1 > max_annot_length then
    Error (Annotation_length max_annot_length)
  else
    let value = String.sub lexeme 1 (String.length lexeme - 1)
    in match lexeme.[0] with
         ':' -> Ok (Annot (Tannot Region.{region; value}))
       | '@' -> Ok (Annot (Vannot Region.{region; value}))
       | '%' -> Ok (Annot (Fannot Region.{region; value}))
       |   _ -> assert false

(* Identifiers *)

let mk_ident' lexeme region lexicon =
  Lexing.from_string lexeme |> scan_ident region lexicon

let mk_ident lexeme region = mk_ident' lexeme region lexicon

(* Predicates *)

let is_string   = function String _ -> true | _ -> false
let is_bytes    = function Bytes _  -> true | _ -> false
let is_int      = function Int _    -> true | _ -> false

let is_ident = function
  Instr _ | Macro _ -> true | _ -> false

let is_annot    = function Annot _  -> true | _ -> false
let is_eof      = function EOF _    -> true | _ -> false

let support_string_delimiter c = c = '"'

let is_sym = function
  SEMI _ | LPAREN _ | RPAREN _ | LBRACE _ | RBRACE _ -> true
| _ -> false

(* END TRAILER *)
}
