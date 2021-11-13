(*
  This file is used throughout the pipeline. Its idea is to add a unique place
  that you have to modify when you add a new operator/constant to the language.

  This file mirrors the LIGO pipeline, starting with Simplify, then Typer and
  ending with Stacking. Usually, when adding a new operator, you'll have to add
  a new constructor at all those places.
*)

module Tree_abstraction = struct

  open Ast_imperative

  module type Constant = sig
    val constants      : string -> rich_constant option
    val constant_to_string      : rich_constant -> string
  end

  let some_const c = Some (Const c)
  let some_deprecated name const = Some (Deprecated {name;const})

  (*
    Each front-end has its owns constants.

    Constants are special names that have their own case in the AST. E_constant
    for regular constants, and T_constant for type constants. Both types are
    defined in `Ast_core/types.ml`.
    For instance, "2 + 2" in Pascaligo is translated to `E_constant ("ADD" , [
      E_literal (Literal_int 2) ;
      E_literal (Literal_int 2) ;
    ])`.

    They are used to represent what can't expressed in the languages:
    - Primitives. Like "int", "string", "unit" for types. Or "+" for values.
    - Tezos specific stuff. Like "operation" for types. Or "source" for values.
    - What can't be represented in the language yet. Like "list" or "List.fold".

    Each constant is expressed as a pair:
    - The left-hand-side is the reserved name in the given front-end.
    - The right-hand-side is the name that will be used in the AST.
  *)
  let pseudo_modules x =
    match x with
    | "Tezos.chain_id"           -> some_const C_CHAIN_ID
    | "Tezos.balance"            -> some_const C_BALANCE
    | "Tezos.now"                -> some_const C_NOW
    | "Tezos.amount"             -> some_const C_AMOUNT
    | "Tezos.sender"             -> some_const C_SENDER
    | "Tezos.address"            -> some_const C_ADDRESS
    | "Tezos.self"               -> some_const C_SELF
    | "Tezos.self_address"       -> some_const C_SELF_ADDRESS
    | "Tezos.implicit_account"   -> some_const C_IMPLICIT_ACCOUNT
    | "Tezos.source"             -> some_const C_SOURCE
    | "Tezos.failwith"           -> some_const C_FAILWITH
    | "Tezos.create_contract"    -> some_const C_CREATE_CONTRACT
    | "Tezos.transaction"        -> some_const C_CALL
    | "Tezos.set_delegate"       -> some_const C_SET_DELEGATE
    | "Tezos.get_contract_with_error" -> some_const C_CONTRACT_WITH_ERROR
    | "Tezos.get_contract_opt"   -> some_const C_CONTRACT_OPT
    | "Tezos.get_entrypoint_opt" -> some_const C_CONTRACT_ENTRYPOINT_OPT
    | "Tezos.level"              -> some_const C_LEVEL
    | "Tezos.pairing_check"      -> some_const C_PAIRING_CHECK
    | "Tezos.never"              -> some_const C_NEVER
    | "Tezos.open_chest"         -> some_const C_OPEN_CHEST
    | "Tezos.call_view"          -> some_const C_VIEW

    (* Sapling *)
    | "Tezos.sapling_empty_state" -> some_const C_SAPLING_EMPTY_STATE
    | "Tezos.sapling_verify_update" -> some_const C_SAPLING_VERIFY_UPDATE

    (* Tickets *)
    | "Tezos.create_ticket" -> some_const C_TICKET
    | "Tezos.join_tickets" -> some_const C_JOIN_TICKET
    | "Tezos.split_ticket" -> some_const C_SPLIT_TICKET
    | "Tezos.read_ticket" -> some_const C_READ_TICKET

    (* Crypto module *)

    | "Crypto.check"    -> some_const C_CHECK_SIGNATURE
    | "Crypto.hash_key" -> some_const C_HASH_KEY
    | "Crypto.blake2b"  -> some_const C_BLAKE2b
    | "Crypto.sha256"   -> some_const C_SHA256
    | "Crypto.sha512"   -> some_const C_SHA512
    | "Crypto.sha3"     -> some_const C_SHA3
    | "Crypto.keccak"   -> some_const C_KECCAK

    (* Bytes module *)

    | "Bytes.pack"   -> some_const C_BYTES_PACK
    | "Bytes.unpack" -> some_const C_BYTES_UNPACK
    | "Bytes.length" -> some_const C_SIZE
    | "Bytes.concat" -> some_const C_CONCAT
    | "Bytes.sub"    -> some_const C_SLICE

    (* Options module *)

    | "Option.unopt"            -> some_const C_UNOPT
    | "Option.unopt_with_error" -> some_const C_UNOPT_WITH_ERROR

    (* List module *)

    | "List.length"   -> some_const C_SIZE
    | "List.size"     -> some_const C_SIZE
    | "List.iter"     -> some_const C_LIST_ITER
    | "List.map"      -> some_const C_LIST_MAP
    | "List.fold"     -> some_const C_LIST_FOLD
    | "List.fold_left"  -> some_const C_LIST_FOLD_LEFT
    | "List.fold_right" -> some_const C_LIST_FOLD_RIGHT
    | "List.head_opt" -> some_const C_LIST_HEAD_OPT
    | "List.tail_opt" -> some_const C_LIST_TAIL_OPT

    (* Set module *)

    | "Set.empty"      -> some_const C_SET_EMPTY
    | "Set.literal"    -> some_const C_SET_LITERAL
    | "Set.cardinal"   -> some_const C_SIZE
    | "Set.mem"        -> some_const C_SET_MEM
    | "Set.add"        -> some_const C_SET_ADD
    | "Set.remove"     -> some_const C_SET_REMOVE
    | "Set.iter"       -> some_const C_SET_ITER
    | "Set.fold"       -> some_const C_SET_FOLD
    | "Set.fold_asc"   -> some_const C_SET_FOLD
    | "Set.fold_desc"  -> some_const C_SET_FOLD_DESC
    | "Set.update"     -> some_const C_SET_UPDATE

    (* Map module *)

    | "Map.find_opt" -> some_const C_MAP_FIND_OPT
    | "Map.update"   -> some_const C_MAP_UPDATE
    | "Map.iter"     -> some_const C_MAP_ITER
    | "Map.map"      -> some_const C_MAP_MAP
    | "Map.fold"     -> some_const C_MAP_FOLD
    | "Map.mem"      -> some_const C_MAP_MEM
    | "Map.size"     -> some_const C_SIZE
    | "Map.add"      -> some_const C_MAP_ADD
    | "Map.remove"   -> some_const C_MAP_REMOVE
    | "Map.empty"    -> some_const C_MAP_EMPTY
    | "Map.literal"  -> some_const C_MAP_LITERAL
    (* Edo linear operator *)
    | "Map.get_and_update" -> some_const C_MAP_GET_AND_UPDATE

    (* Big_map module *)

    | "Big_map.find"     -> some_const C_MAP_FIND
    | "Big_map.find_opt" -> some_const C_MAP_FIND_OPT
    | "Big_map.update"   -> some_const C_MAP_UPDATE
    | "Big_map.literal"  -> some_const C_BIG_MAP_LITERAL
    | "Big_map.empty"    -> some_const C_BIG_MAP_EMPTY
    | "Big_map.mem"      -> some_const C_MAP_MEM
    | "Big_map.remove"   -> some_const C_MAP_REMOVE
    | "Big_map.add"      -> some_const C_MAP_ADD
    (* Edo linear operator *)
    | "Big_map.get_and_update" -> some_const C_BIG_MAP_GET_AND_UPDATE

    (* Bitwise module *)

    | "Bitwise.or"          -> some_const C_OR
    | "Bitwise.and"         -> some_const C_AND
    | "Bitwise.xor"         -> some_const C_XOR
    | "Bitwise.shift_left"  -> some_const C_LSL
    | "Bitwise.shift_right" -> some_const C_LSR

    (* String module *)

    | "String.length"   -> some_const C_SIZE
    | "String.size"     -> some_deprecated x C_SIZE  (* Deprecated *)
    | "String.slice"    -> some_deprecated x C_SLICE (* Deprecated *)
    | "String.sub"      -> some_const C_SLICE
    | "String.concat"   -> some_const C_CONCAT

    (* Testing module *)

    | "Test.originate" -> some_const C_TEST_ORIGINATE
    | "Test.originate_from_file" -> some_const C_TEST_ORIGINATE_FROM_FILE
    | "Test.set_now" -> some_const C_TEST_SET_NOW
    | "Test.set_source" -> some_const C_TEST_SET_SOURCE
    | "Test.set_baker" -> some_const C_TEST_SET_BAKER
    | "Test.transfer_to_contract" -> some_const C_TEST_EXTERNAL_CALL_TO_CONTRACT
    | "Test.transfer_to_contract_exn" -> some_const C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN
    | "Test.transfer" -> some_const C_TEST_EXTERNAL_CALL_TO_ADDRESS
    | "Test.transfer_exn" -> some_const C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN
    | "Test.get_storage" -> some_const C_TEST_GET_STORAGE
    | "Test.get_storage_of_address" -> some_const C_TEST_GET_STORAGE_OF_ADDRESS
    | "Test.get_balance" -> some_const C_TEST_GET_BALANCE
    | "Test.michelson_equal" -> some_const C_TEST_MICHELSON_EQUAL
    | "Test.log" -> some_const C_TEST_LOG
    | "Test.reset_state" -> some_const C_TEST_STATE_RESET
    | "Test.bootstrap_contract" -> some_const C_TEST_BOOTSTRAP_CONTRACT
    | "Test.nth_bootstrap_contract" -> some_const C_TEST_NTH_BOOTSTRAP_CONTRACT
    | "Test.nth_bootstrap_account" -> some_const C_TEST_GET_NTH_BS
    | "Test.last_originations" -> some_const C_TEST_LAST_ORIGINATIONS
    | "Test.compile_value" -> some_const C_TEST_COMPILE_META_VALUE
    | "Test.mutate_value" -> some_const C_TEST_MUTATE_VALUE
    | "Test.mutation_test" -> some_const C_TEST_MUTATION_TEST
    | "Test.mutation_test_all" -> some_const C_TEST_MUTATION_TEST_ALL
    | "Test.save_mutation" -> some_const C_TEST_SAVE_MUTATION
    | "Test.run" -> some_const C_TEST_RUN
    | "Test.eval" -> some_const C_TEST_EVAL
    | "Test.compile_contract" -> some_const C_TEST_COMPILE_CONTRACT
    | "Test.to_contract" -> some_const C_TEST_TO_CONTRACT
    | "Test.nth_bootstrap_typed_address" -> some_const C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS
    | "Test.to_entrypoint" -> some_const C_TEST_TO_ENTRYPOINT
    | "Test.to_typed_address" -> some_const C_TEST_TO_TYPED_ADDRESS
    | "Test.set_big_map" -> some_const C_TEST_SET_BIG_MAP
    | "Test.cast_address" -> some_const C_TEST_CAST_ADDRESS
    | "Test.create_chest" -> some_const C_TEST_CREATE_CHEST
    | "Test.create_chest_key" -> some_const C_TEST_CREATE_CHEST_KEY

    (* Operator module *)

    | "Operator.neg"   -> some_const C_NEG
    | "Operator.add"   -> some_const C_ADD
    | "Operator.sub"   -> some_const C_SUB
    | "Operator.times" -> some_const C_MUL
    | "Operator.div"   -> some_const C_DIV
    | "Operator.modulus" -> some_const C_MOD
    | "Operator.eq"    -> some_const C_EQ
    | "Operator.not"   -> some_const C_NOT
    | "Operator.and"   -> some_const C_AND
    | "Operator.or"    -> some_const C_OR
    | "Operator.gt"    -> some_const C_GT
    | "Operator.ge"    -> some_const C_GE
    | "Operator.lt"    -> some_const C_LT
    | "Operator.le"    -> some_const C_LE
    | "Operator.cons"  -> some_const C_CONS
    | "Operator.neq"   -> some_const C_NEQ

    | _ -> None


  let pseudo_module_to_string = function
    | C_CHAIN_ID                -> "Tezos.chain_id"
    | C_BALANCE                 -> "Tezos.balance"
    | C_NOW                     -> "Tezos.now"
    | C_AMOUNT                  -> "Tezos.amount"
    | C_SENDER                  -> "Tezos.sender"
    | C_ADDRESS                 -> "Tezos.address"
    | C_SELF                    -> "Tezos.self"
    | C_SELF_ADDRESS            -> "Tezos.self_address"
    | C_IMPLICIT_ACCOUNT        -> "Tezos.implicit_account"
    | C_SOURCE                  -> "Tezos.source"
    | C_FAILWITH                -> "Tezos.failwith"
    | C_CREATE_CONTRACT         -> "Tezos.create_contract"
    | C_CALL                    -> "Tezos.transaction"
    | C_SET_DELEGATE            -> "Tezos.set_delegate"
    | C_CONTRACT_WITH_ERROR     -> "Tezos.get_contract_with_error"
    | C_CONTRACT_OPT            -> "Tezos.get_contract_opt"
    | C_CONTRACT_ENTRYPOINT_OPT -> "Tezos.get_entrypoint_opt"
    | C_CONTRACT                -> "Tezos.get_contract"
    | C_CONTRACT_ENTRYPOINT     -> "Tezos.get_entrypoint"
    | C_NEVER                   -> "Tezos.never"
    | C_OPEN_CHEST              -> "Tezos.open_chest" 
    | C_VIEW                    -> "Tezos.call_view" 

    (* Operator module *)

    | C_NEG  -> "Operator.neg"
    | C_ADD  -> "Operator.add"
    | C_SUB  -> "Operator.sub"
    | C_MUL  -> "Operator.times"
    | C_DIV  -> "Operator.div"
    | C_MOD  -> "Operator.modulus"
    | C_EQ   -> "Operator.eq"
    | C_NOT  -> "Operator.not"
    | C_AND  -> "Operator.and"
    | C_OR   -> "Operator.or"
    | C_GT   -> "Operator.gt"
    | C_GE   -> "Operator.ge"
    | C_LT   -> "Operator.lt"
    | C_LE   -> "Operator.le"
    | C_CONS -> "Operator.cons"
    | C_NEQ  -> "Operator.neq"

    (* Crypto module *)

    | C_CHECK_SIGNATURE -> "Crypto.check"
    | C_HASH_KEY        -> "Crypto.hash_key"
    | C_BLAKE2b         -> "Crypto.blake2b"
    | C_SHA256          -> "Crypto.sha256"
    | C_SHA512          -> "Crypto.sha512"
    | C_SHA3            -> "Crypto.sha3"
    | C_KECCAK          -> "Crypto.keccak"

    (* Bytes module *)

    | C_BYTES_PACK   -> "Bytes.pack"
    | C_BYTES_UNPACK -> "Bytes.unpack"
    | C_SIZE         -> "Bytes.length"
    | C_CONCAT       -> "Bytes.concat"
    | C_SLICE        -> "Bytes.sub"

    (* List module *)

    (*  | C_SIZE      -> "List.size" *)
    | C_LIST_ITER -> "List.iter"
    | C_LIST_MAP  -> "List.map"
    | C_LIST_FOLD -> "List.fold"
    | C_LIST_FOLD_LEFT -> "List.fold_left"
    | C_LIST_FOLD_RIGHT -> "List.fold_right"

    (* Set module *)

    | C_SET_EMPTY      -> "Set.empty"
    | C_SET_LITERAL    -> "Set.literal"
    (* | C_SIZE        -> "Set.cardinal"*)
    | C_SET_MEM        -> "Set.mem"
    | C_SET_ADD        -> "Set.add"
    | C_SET_REMOVE     -> "Set.remove"
    | C_SET_ITER       -> "Set.iter"
    | C_SET_FOLD       -> "Set.fold"
    | C_SET_FOLD_DESC -> "Set.fold_right"
    | C_SET_UPDATE     -> "Set.update"

    (* Map module *)

    | C_MAP_FIND_OPT -> "Map.find_opt"
    | C_MAP_UPDATE   -> "Map.update"
    | C_MAP_ITER     -> "Map.iter"
    | C_MAP_MAP      -> "Map.map"
    | C_MAP_FOLD     -> "Map.fold"
    | C_MAP_MEM      -> "Map.mem"
    (*  | C_SIZE         -> "Map.size" *)
    | C_MAP_ADD      -> "Map.add"
    | C_MAP_REMOVE   -> "Map.remove"
    | C_MAP_EMPTY    -> "Map.empty"
    | C_MAP_LITERAL  -> "Map.literal"

    (* Big_map module *)

    | C_MAP_FIND        -> "Big_map.find"
    (*  | C_MAP_FIND_OPT    -> "Big_map.find_opt"
        | C_MAP_UPDATE      -> "Big_map.update" *)
    | C_BIG_MAP_LITERAL -> "Big_map.literal"
    | C_BIG_MAP_EMPTY   -> "Big_map.empty"
    (*  | C_MAP_MEM         -> "Big_map.mem"
        | C_MAP_REMOVE      -> "Big_map.remove"
        | C_MAP_ADD         -> "Big_map.add" *)

    (* Bitwise module *)

    (* | C_OR  -> "Bitwise.or" (* will never trigger *)
     * | C_AND -> "Bitwise.and" *)
    | C_XOR -> "Bitwise.xor"
    | C_LSL -> "Bitwise.shift_left"
    | C_LSR -> "Bitwise.shift_right"

    (* String module *)

    (*  | C_SIZE   -> "String.length" (* will never trigger, rename size *)
        | C_SLICE  -> "String.sub"
        | C_CONCAT -> "String.concat" *)

    (* Not parsed *)
    | C_SOME -> "Some"
    | C_NONE -> "None"

    (* Testing module *)

    | C_TEST_ORIGINATE -> "Test.originate"
    | C_TEST_ORIGINATE_FROM_FILE -> "Test.originate_from_file"
    | C_TEST_SET_NOW -> "Test.set_now"
    | C_TEST_SET_SOURCE -> "Test.set_source"
    | C_TEST_SET_BAKER -> "Test.set_baker"
    | C_TEST_EXTERNAL_CALL_TO_CONTRACT -> "Test.transfer_to_contract"
    | C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN -> "Test.transfer_to_contract_exn"
    | C_TEST_EXTERNAL_CALL_TO_ADDRESS -> "Test.transfer"
    | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN -> "Test.transfer_exn"
    | C_TEST_GET_STORAGE -> "Test.get_storage"
    | C_TEST_GET_STORAGE_OF_ADDRESS -> "Test.get_storage_of_address"
    | C_TEST_GET_BALANCE -> "Test.get_balance"
    | C_TEST_MICHELSON_EQUAL -> "Test.michelson_equal"
    | C_TEST_LOG -> "Test.log"
    | C_TEST_STATE_RESET -> "Test.reset_state"
    | C_TEST_BOOTSTRAP_CONTRACT -> "Test.bootstrap_contract"
    | C_TEST_NTH_BOOTSTRAP_CONTRACT -> "Test.nth_bootstrap_contract"
    | C_TEST_GET_NTH_BS -> "Test.nth_bootstrap_account"
    | C_TEST_LAST_ORIGINATIONS -> "Test.last_originations"
    | C_TEST_COMPILE_META_VALUE -> "Test.compile_value"
    | C_TEST_MUTATE_COUNT -> "Test.mutate_count"
    | C_TEST_MUTATE_VALUE -> "Test.mutate_value"
    | C_TEST_MUTATION_TEST -> "Test.mutation_test"
    | C_TEST_MUTATION_TEST_ALL -> "Test.mutation_test_all"
    | C_TEST_SAVE_MUTATION -> "Test.save_mutation"
    | C_TEST_RUN -> "Test.run"
    | C_TEST_EVAL -> "Test.eval"
    | C_TEST_COMPILE_CONTRACT -> "Test.compile_contract"
    | C_TEST_TO_CONTRACT -> "Test.to_contract"
    | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS -> "Test.nth_bootstrap_typed_address"
    | C_TEST_TO_ENTRYPOINT -> "Test.to_entrypoint"
    | C_TEST_TO_TYPED_ADDRESS -> "Test.to_typed_address"
    | C_TEST_SET_BIG_MAP -> "Test.set_big_map"
    | C_TEST_CAST_ADDRESS -> "Test.cast_address"
    | C_TEST_CREATE_CHEST -> "Test.create_chest"


    | _ as c -> failwith @@ Format.asprintf "Constant not handled : %a" Stage_common.PP.constant' c


  module Pascaligo = struct
    let constants x =
      let some_deprecated = some_deprecated x in
      match x with
      (* Tezos module (ex-Michelson) *)
      | "chain_id"               -> some_deprecated C_CHAIN_ID            (* Deprecated *)
      | "get_chain_id"           -> some_deprecated C_CHAIN_ID            (* Deprecated *)
      | "balance"                -> some_deprecated C_BALANCE             (* Deprecated *)
      | "now"                    -> some_deprecated C_NOW                 (* Deprecated *)
      | "amount"                 -> some_deprecated C_AMOUNT              (* Deprecated *)
      | "sender"                 -> some_deprecated C_SENDER              (* Deprecated *)
      | "address"                -> some_deprecated C_ADDRESS             (* Deprecated *)
      | "self_address"           -> some_deprecated C_SELF_ADDRESS        (* Deprecated *)
      | "implicit_account"       -> some_deprecated C_IMPLICIT_ACCOUNT    (* Deprecated *)
      | "source"                 -> some_deprecated C_SOURCE              (* Deprecated *)
      | "failwith"               -> some_const      C_FAILWITH
      | "transaction"            -> some_deprecated C_CALL                    (* Deprecated *)
      | "set_delegate"           -> some_deprecated C_SET_DELEGATE            (* Deprecated *)
      | "get_contract"           -> some_deprecated C_CONTRACT                (* Deprecated *)
      | "get_contract_opt"       -> some_deprecated C_CONTRACT_OPT            (* Deprecated *)
      | "get_entrypoint"         -> some_deprecated C_CONTRACT_ENTRYPOINT     (* Deprecated *)
      | "get_entrypoint_opt"     -> some_deprecated C_CONTRACT_ENTRYPOINT_OPT (* Deprecated *)

      | "Michelson.is_nat" -> some_deprecated C_IS_NAT  (* Deprecated *)
      | "is_nat"           -> some_const C_IS_NAT
      | "int"              -> some_const C_INT
      | "abs"              -> some_const C_ABS
      | "ediv"             -> some_const C_EDIV
      | "unit"             -> some_const C_UNIT

      | "cons"             -> some_deprecated C_CONS (* Deprecated *)

      (* Crypto module *)

      | "crypto_check"    -> some_deprecated C_CHECK_SIGNATURE       (* Deprecated *)
      | "crypto_hash_key" -> some_deprecated C_HASH_KEY              (* Deprecated *)
      | "blake2b"         -> some_deprecated C_BLAKE2b               (* Deprecated *)
      | "sha_256"         -> some_deprecated C_SHA256                (* Deprecated *)
      | "sha_512"         -> some_deprecated C_SHA512                (* Deprecated *)

      (* Bytes module *)

      | "bytes_pack"   -> some_deprecated C_BYTES_PACK    (* Deprecated *)
      | "bytes_unpack" -> some_deprecated C_BYTES_UNPACK  (* Deprecated *)
      | "Bytes.size"   -> some_deprecated C_SIZE          (* Deprecated *)
      | "bytes_concat" -> some_deprecated C_CONCAT        (* Deprecated *)
      | "bytes_slice"  -> some_deprecated C_SLICE         (* Deprecated *)
      | "Bytes.slice"  -> some_deprecated C_SLICE         (* Deprecated *)

      (* List module *)

      | "list_size"   -> some_deprecated C_SIZE       (* Deprecated *)
      | "list_iter"   -> some_deprecated C_LIST_ITER  (* Deprecated *)
      | "list_map"    -> some_deprecated C_LIST_MAP   (* Deprecated *)
      | "list_fold"   -> some_deprecated C_LIST_FOLD  (* Deprecated *)

      (* Set module *)


      | "Set.size"    -> some_deprecated C_SIZE        (* Deprecated *)
      | "set_size"    -> some_deprecated C_SIZE        (* Deprecated *)
      | "set_empty"   -> some_deprecated C_SET_EMPTY   (* Deprecated *)
      | "set_mem"     -> some_deprecated C_SET_MEM     (* Deprecated *)
      | "set_add"     -> some_deprecated C_SET_ADD     (* Deprecated *)
      | "set_remove"  -> some_deprecated C_SET_REMOVE  (* Deprecated *)
      | "set_iter"    -> some_deprecated C_SET_ITER    (* Deprecated *)
      | "set_fold"    -> some_deprecated C_SET_FOLD    (* Deprecated *)

      (* Map module *)

      | "get_force"    -> some_deprecated C_MAP_FIND      (* Deprecated *)
      | "map_get"      -> some_deprecated C_MAP_FIND_OPT  (* Deprecated *)
      | "map_update"   -> some_deprecated C_MAP_UPDATE    (* Deprecated *)
      | "map_remove"   -> some_deprecated C_MAP_REMOVE    (* Deprecated *)
      | "map_iter"     -> some_deprecated C_MAP_ITER      (* Deprecated *)
      | "map_map"      -> some_deprecated C_MAP_MAP       (* Deprecated *)
      | "map_fold"     -> some_deprecated C_MAP_FOLD      (* Deprecated *)
      | "map_mem"      -> some_deprecated C_MAP_MEM       (* Deprecated *)
      | "map_size"     -> some_deprecated C_SIZE          (* Deprecated *)


      (* Bitwise module *)

      | "bitwise_or"          -> some_deprecated C_OR      (* Deprecated *)
      | "bitwise_and"         -> some_deprecated C_AND     (* Deprecated *)
      | "bitwise_xor"         -> some_deprecated C_XOR     (* Deprecated *)
      | "bitwise_lsl"         -> some_deprecated C_LSL     (* Deprecated *)
      | "bitwise_lsr"         -> some_deprecated C_LSR     (* Deprecated *)

      (* String module *)

      | "string_slice"    -> some_deprecated C_SLICE    (* Deprecated *)
      | "string_concat"   -> some_deprecated C_CONCAT   (* Deprecated *)

      (* Others *)

      | "assert"                 -> some_const C_ASSERTION
      | "assert_with_error"      -> some_const C_ASSERTION_WITH_ERROR
      | "assert_some"            -> some_const C_ASSERT_SOME
      | "assert_some_with_error" -> some_const C_ASSERT_SOME_WITH_ERROR
      | "size"                   -> some_deprecated C_SIZE (* Deprecated *)

      | _ as c                   -> pseudo_modules c

    let constant'_to_string = function
      (* Tezos module (ex-Michelson) *)
      | C_FAILWITH -> "failwith"

      | C_IS_NAT     -> "is_nat"
      | C_INT        -> "int"
      | C_ABS        -> "abs"
      | C_EDIV       -> "ediv"
      | C_UNIT       -> "unit"
      | C_LIST_EMPTY -> "nil"

      (*->  Others *)

      | C_ASSERTION   -> "assert"
      | C_ASSERT_SOME -> "assert_some"

      | _ as c            -> pseudo_module_to_string c

    let constant_to_string = function
      | Deprecated {name;_} -> name
      | Const x -> constant'_to_string x

  end

  module Cameligo = struct
    let constants x =
      let some_deprecated = some_deprecated x in
      match x with
      (* Tezos (ex-Michelson, ex-Current, ex-Operation) *)

      | "chain_id"                   -> some_deprecated C_CHAIN_ID            (* Deprecated *)
      | "Current.balance"            -> some_deprecated C_BALANCE             (* Deprecated *)
      | "balance"                    -> some_deprecated C_BALANCE             (* Deprecated *)
      | "Current.time"               -> some_deprecated C_NOW                 (* Deprecated *)
      | "time"                       -> some_deprecated C_NOW                 (* Deprecated *)
      | "Current.amount"             -> some_deprecated C_AMOUNT              (* Deprecated *)
      | "amount"                     -> some_deprecated C_AMOUNT              (* Deprecated *)
      | "Current.sender"             -> some_deprecated C_SENDER              (* Deprecated *)
      | "sender"                     -> some_deprecated C_SENDER              (* Deprecated *)
      | "Current.address"            -> some_deprecated C_ADDRESS             (* Deprecated *)
      | "Current.self_address"       -> some_deprecated C_SELF_ADDRESS        (* Deprecated *)
      | "Current.implicit_account"   -> some_deprecated C_IMPLICIT_ACCOUNT    (* Deprecated *)
      | "Current.source"             -> some_deprecated C_SOURCE              (* Deprecated *)
      | "source"                     -> some_deprecated C_SOURCE              (* Deprecated *)
      | "Current.failwith"           -> some_deprecated C_FAILWITH            (* Deprecated *)
      | "failwith"                   -> some_const C_FAILWITH

      | "Operation.transaction"        -> some_deprecated C_CALL              (* Deprecated *)
      | "Operation.set_delegate"       -> some_deprecated C_SET_DELEGATE      (* Deprecated *)
      | "Operation.get_contract"       -> some_deprecated C_CONTRACT          (* Deprecated *)
      | "Operation.get_contract_opt"   -> some_deprecated C_CONTRACT_OPT      (* Deprecated *)
      | "Operation.get_entrypoint"     -> some_deprecated C_CONTRACT_ENTRYPOINT (* Deprecated *)
      | "Operation.get_entrypoint_opt" -> some_deprecated C_CONTRACT_ENTRYPOINT_OPT (* Deprecated *)

      | "Michelson.is_nat" -> some_deprecated C_IS_NAT  (* Deprecated *)
      | "is_nat"           -> some_const C_IS_NAT
      | "int"              -> some_const C_INT
      | "abs"              -> some_const C_ABS
      | "ediv"             -> some_const C_EDIV
      | "unit"             -> some_const C_UNIT

      (* Bytes module *)

      | "Bytes.size"   -> some_deprecated C_SIZE       (* Deprecated *)
      | "Bytes.slice"  -> some_deprecated C_SLICE      (* Deprecated *)

      (* Set module *)
      | "Set.size"     -> some_deprecated C_SIZE (* Deprecated *)

      (* Map module *)
      | "Map.find"     -> some_deprecated C_MAP_FIND     (* Deprecated *)

      (* Bitwise module *)

      | "Bitwise.lor"         -> some_deprecated C_OR  (* Deprecated *)
      | "Bitwise.land"        -> some_deprecated C_AND (* Deprecated *)
      | "Bitwise.lxor"        -> some_deprecated C_XOR (* Deprecated *)

      (* Loop module *)

      | "Loop.fold_while" -> some_deprecated C_FOLD_WHILE    (* Deprecated *)
      | "Loop.resume"     -> some_deprecated C_FOLD_CONTINUE (* Deprecated *)
      | "continue"        -> some_deprecated C_FOLD_CONTINUE (* Deprecated *)
      | "Loop.stop"       -> some_deprecated C_FOLD_STOP     (* Deprecated *)
      | "stop"            -> some_deprecated C_FOLD_STOP     (* Deprecated *)

      (* Others *)

      | "assert"                 -> some_const C_ASSERTION
      | "assert_with_error"      -> some_const C_ASSERTION_WITH_ERROR
      | "assert_some"            -> some_const C_ASSERT_SOME
      | "assert_some_with_error" -> some_const C_ASSERT_SOME_WITH_ERROR
      | "true"         -> some_const C_TRUE
      | "false"        -> some_const C_FALSE

      | _ as c -> pseudo_modules c

    let constant'_to_string = function
      (* Tezos (ex-Michelson, ex-Current, ex-Operation) *)
      | C_FAILWITH -> "failwith"

      | C_IS_NAT     -> "is_nat"
      | C_INT        -> "int"
      | C_ABS        -> "abs"
      | C_EDIV       -> "ediv"
      | C_UNIT       -> "unit"
      | C_LIST_EMPTY -> "[]"

      (* Others *)

      | C_ASSERTION   -> "assert"
      | C_ASSERT_SOME -> "assert_some"
      | C_TRUE -> "true"
      | C_FALSE -> "false"

      | _ as c -> pseudo_module_to_string c

    let constant_to_string = function
      | Deprecated {name;_} -> name
      | Const x -> constant'_to_string x

  end

  module Reasonligo = struct
    let constants x =
      let some_deprecated = some_deprecated x in
      match x with
      (* Tezos (ex-Michelson, ex-Current, ex-Operation) *)

      | "chain_id"                   -> some_deprecated C_CHAIN_ID            (* Deprecated *)
      | "Current.balance"            -> some_deprecated C_BALANCE             (* Deprecated *)
      | "balance"                    -> some_deprecated C_BALANCE             (* Deprecated *)
      | "Current.time"               -> some_deprecated C_NOW                 (* Deprecated *)
      | "time"                       -> some_deprecated C_NOW                 (* Deprecated *)
      | "Current.amount"             -> some_deprecated C_AMOUNT              (* Deprecated *)
      | "amount"                     -> some_deprecated C_AMOUNT              (* Deprecated *)
      | "Current.sender"             -> some_deprecated C_SENDER              (* Deprecated *)
      | "sender"                     -> some_deprecated C_SENDER              (* Deprecated *)
      | "Current.address"            -> some_deprecated C_ADDRESS             (* Deprecated *)
      | "Current.self_address"       -> some_deprecated C_SELF_ADDRESS        (* Deprecated *)
      | "Current.implicit_account"   -> some_deprecated C_IMPLICIT_ACCOUNT    (* Deprecated *)
      | "Current.source"             -> some_deprecated C_SOURCE              (* Deprecated *)
      | "source"                     -> some_deprecated C_SOURCE              (* Deprecated *)
      | "Current.failwith"           -> some_deprecated C_FAILWITH            (* Deprecated *)
      | "failwith"                   -> some_const C_FAILWITH

      | "Operation.transaction"        -> some_deprecated C_CALL              (* Deprecated *)
      | "Operation.set_delegate"       -> some_deprecated C_SET_DELEGATE      (* Deprecated *)
      | "Operation.get_contract"       -> some_deprecated C_CONTRACT          (* Deprecated *)
      | "Operation.get_contract_opt"   -> some_deprecated C_CONTRACT_OPT      (* Deprecated *)
      | "Operation.get_entrypoint"     -> some_deprecated C_CONTRACT_ENTRYPOINT (* Deprecated *)
      | "Operation.get_entrypoint_opt" -> some_deprecated C_CONTRACT_ENTRYPOINT_OPT (* Deprecated *)

      | "Michelson.is_nat" -> some_deprecated C_IS_NAT  (* Deprecated *)
      | "is_nat"           -> some_const C_IS_NAT
      | "int"              -> some_const C_INT
      | "abs"              -> some_const C_ABS
      | "ediv"             -> some_const C_EDIV
      | "unit"             -> some_const C_UNIT

      (* Bytes module *)

      | "Bytes.size"   -> some_deprecated C_SIZE       (* Deprecated *)
      | "Bytes.slice"  -> some_deprecated C_SLICE      (* Deprecated *)

      (* Set module *)
      | "Set.size"     -> some_deprecated C_SIZE (* Deprecated *)

      (* Map module *)
      | "Map.find"     -> some_deprecated C_MAP_FIND     (* Deprecated *)

      (* Bitwise module *)

      | "Bitwise.lor"         -> some_deprecated C_OR  (* Deprecated *)
      | "Bitwise.land"        -> some_deprecated C_AND (* Deprecated *)
      | "Bitwise.lxor"        -> some_deprecated C_XOR (* Deprecated *)

      (* Loop module *)

      | "Loop.fold_while" -> some_deprecated C_FOLD_WHILE    (* Deprecated *)
      | "Loop.resume"     -> some_deprecated C_FOLD_CONTINUE (* Deprecated *)
      | "continue"        -> some_deprecated C_FOLD_CONTINUE (* Deprecated *)
      | "Loop.stop"       -> some_deprecated C_FOLD_STOP     (* Deprecated *)
      | "stop"            -> some_deprecated C_FOLD_STOP     (* Deprecated *)

      (* Others *)

      | "assert"                 -> some_const C_ASSERTION
      | "assert_with_error"      -> some_const C_ASSERTION_WITH_ERROR
      | "assert_some"            -> some_const C_ASSERT_SOME
      | "assert_some_with_error" -> some_const C_ASSERT_SOME_WITH_ERROR
      | "true"         -> some_const C_TRUE
      | "false"        -> some_const C_FALSE

      | _ as c -> pseudo_modules c

    let constant'_to_string = function
      (* Tezos (ex-Michelson, ex-Current, ex-Operation) *)
      | C_FAILWITH -> "failwith"

      | C_IS_NAT     -> "is_nat"
      | C_INT        -> "int"
      | C_ABS        -> "abs"
      | C_EDIV       -> "ediv"
      | C_UNIT       -> "unit"
      | C_LIST_EMPTY -> "[]"

      (* Others *)

      | C_ASSERTION   -> "assert"
      | C_ASSERT_SOME -> "assert_some"
      | C_TRUE -> "true"
      | C_FALSE -> "false"

      | _ as c -> pseudo_module_to_string c

    let constant_to_string = function
      | Deprecated {name;_} -> name
      | Const x -> constant'_to_string x

  end

  module Jsligo = Reasonligo
end

module Stacking = struct
  (*
    Most constants pass through the Spilling unchanged. So they need to be
    compiled down to Michelson. This is the last step.

    When compiling the constant, we need to provide its arity (through the type
    predicate, defined in `Helpers.Stacking`, and its michelson code.
    In the case of an n-ary constant, we assume that the stack has the form:
    `x1 :: x2 :: x3 ... :: xn :: _`.

    This step requires knowledge of Michelson. Knowledge of
    `Tezos_utils.Michelson` will help too, so that no Michelson has to actually
    be written by hand.
   *)
  type protocol_type = Environment.Protocols.t
  include Helpers.Stacking
  open Tezos_utils.Michelson
  open Stage_common.Types

  let get_operators (protocol_version: protocol_type) c : predicate option =
    match c , protocol_version with
    | C_ADD                , _   -> Some ( simple_binary @@ prim "ADD")
    | C_SUB                , _   -> Some ( simple_binary @@ prim "SUB")
    | C_MUL                , _   -> Some ( simple_binary @@ prim "MUL")
    | C_EDIV               , _   -> Some ( simple_binary @@ prim "EDIV")
    | C_DIV                , _   -> Some ( simple_binary @@ seq [prim "EDIV" ; i_assert_some_msg (i_push_string "DIV by 0") ; i_car])
    | C_MOD                , _   -> Some ( simple_binary @@ seq [prim "EDIV" ; i_assert_some_msg (i_push_string "MOD by 0") ; i_cdr])
    | C_NEG                , _   -> Some ( simple_unary @@ prim "NEG")
    | C_OR                 , _   -> Some ( simple_binary @@ prim "OR")
    | C_AND                , _   -> Some ( simple_binary @@ prim "AND")
    | C_XOR                , _   -> Some ( simple_binary @@ prim "XOR")
    | C_LSL                , _   -> Some ( simple_binary @@ prim "LSL")
    | C_LSR                , _   -> Some ( simple_binary @@ prim "LSR")
    | C_NOT                , _   -> Some ( simple_unary @@ prim "NOT")
    | C_PAIR               , _   -> Some ( simple_binary @@ prim "PAIR")
    | C_CAR                , _   -> Some ( simple_unary @@ prim "CAR")
    | C_CDR                , _   -> Some ( simple_unary @@ prim "CDR")
    | C_TRUE               , _   -> Some ( simple_constant @@ i_push (prim "bool") (prim "True"))
    | C_FALSE              , _   -> Some ( simple_constant @@ i_push (prim "bool") (prim "False"))
    | C_EQ                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "EQ"])
    | C_NEQ                , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "NEQ"])
    | C_LT                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "LT"])
    | C_LE                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "LE"])
    | C_GT                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "GT"])
    | C_GE                 , _   -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "GE"])
    | C_UPDATE             , _   -> Some ( simple_ternary @@ prim "UPDATE")
    | C_SOME               , _   -> Some ( simple_unary  @@ prim "SOME")
    | C_MAP_FIND           , _   -> Some ( simple_binary @@ seq [prim "GET" ; i_assert_some_msg (i_push_string "MAP FIND")])
    | C_MAP_MEM            , _   -> Some ( simple_binary @@ prim "MEM")
    | C_MAP_FIND_OPT       , _   -> Some ( simple_binary @@ prim "GET")
    | C_MAP_ADD            , _   -> Some ( simple_ternary @@ seq [dip (i_some) ; prim "UPDATE"])
    | C_MAP_UPDATE         , _   -> Some ( simple_ternary @@ prim "UPDATE")
    | (C_MAP_GET_AND_UPDATE|C_BIG_MAP_GET_AND_UPDATE) , _ ->
      Some (simple_ternary @@ seq [prim "GET_AND_UPDATE"; prim "PAIR"])
    | C_FOLD_WHILE         , _   ->
      Some ( simple_binary @@ seq [i_swap ; (i_push (prim "bool") (prim "True"));prim ~children:[seq [dip i_dup; i_exec; i_unpair]] "LOOP" ;i_swap ; i_drop])
    | C_FOLD_CONTINUE         , _   -> Some ( simple_unary @@ seq [(i_push (prim "bool") (prim "True")); i_pair])
    | C_FOLD_STOP             , _   -> Some ( simple_unary @@ seq [(i_push (prim "bool") (prim "False")); i_pair])
    | C_SIZE                  , _   -> Some ( simple_unary @@ prim "SIZE")
    | C_FAILWITH              , _   -> Some ( simple_unary @@ prim "FAILWITH")
    | C_NEVER                 , _   -> Some ( simple_unary @@ prim "NEVER")
    | C_UNOPT                 , _   -> Some ( simple_binary @@ i_if_none (seq [i_push_string "option is None"; i_failwith]) (seq []))
    | C_UNOPT_WITH_ERROR      , _   -> Some ( simple_binary @@ i_if_none (i_failwith) (seq [ i_swap; i_drop]))
    | C_ASSERT_SOME           , _   -> Some ( simple_unary @@ i_if_none (seq [i_push_string "failed assert some" ; i_failwith]) (seq [i_drop; i_push_unit]))
    | C_ASSERT_SOME_WITH_ERROR, _   -> Some ( simple_binary @@ i_if_none (i_failwith) (seq [i_dropn 2; i_push_unit]))
    | C_ASSERT_INFERRED    , _   -> Some ( simple_binary @@ i_if (seq [i_failwith]) (seq [i_drop ; i_push_unit]))
    | C_ASSERTION          , _   -> Some ( simple_unary @@ i_if (seq [i_push_unit]) (seq [i_push_string "failed assertion" ; i_failwith]))
    | C_ASSERTION_WITH_ERROR, _  -> Some ( simple_binary @@ i_if (seq [i_drop; i_push_unit]) (i_failwith))
    | C_INT                , _   -> Some ( simple_unary @@ prim "INT")
    | C_ABS                , _   -> Some ( simple_unary @@ prim "ABS")
    | C_IS_NAT             , _   -> Some ( simple_unary @@ prim "ISNAT")
    | C_CONS               , _   -> Some ( simple_binary @@ prim "CONS")
    | C_UNIT               , _   -> Some ( simple_constant @@ prim "UNIT")
    | C_BALANCE            , _   -> Some ( simple_constant @@ prim "BALANCE")
    | C_AMOUNT             , _   -> Some ( simple_constant @@ prim "AMOUNT")
    | C_ADDRESS            , _   -> Some ( simple_unary @@ prim "ADDRESS")
    | C_SELF_ADDRESS       , _   -> Some ( simple_constant @@ seq [prim "SELF_ADDRESS"])
    | C_IMPLICIT_ACCOUNT   , _   -> Some ( simple_unary @@ prim "IMPLICIT_ACCOUNT")
    | C_SET_DELEGATE       , _   -> Some ( simple_unary @@ prim "SET_DELEGATE")
    | C_NOW                , _   -> Some ( simple_constant @@ prim "NOW")
    | C_CALL               , _   -> Some ( simple_ternary @@ prim "TRANSFER_TOKENS")
    | C_SOURCE             , _   -> Some ( simple_constant @@ prim "SOURCE")
    | C_SENDER             , _   -> Some ( simple_constant @@ prim "SENDER")
    | C_SET_MEM            , _   -> Some ( simple_binary @@ prim "MEM")
    | C_SET_ADD            , _   -> Some ( simple_binary @@ seq [dip (i_push (prim "bool") (prim "True")) ; prim "UPDATE"])
    | C_SET_REMOVE         , _   -> Some ( simple_binary @@ seq [dip (i_push (prim "bool") (prim "False")) ; prim "UPDATE"])
    | C_SET_UPDATE         , _   -> Some ( simple_ternary @@ prim "UPDATE" )
    | C_SLICE              , _   -> Some ( simple_ternary @@ seq [prim "SLICE" ; i_assert_some_msg (i_push_string "SLICE")])
    | C_SHA256             , _   -> Some ( simple_unary @@ prim "SHA256")
    | C_SHA512             , _   -> Some ( simple_unary @@ prim "SHA512")
    | C_BLAKE2b            , _   -> Some ( simple_unary @@ prim "BLAKE2B")
    | C_CHECK_SIGNATURE    , _   -> Some ( simple_ternary @@ prim "CHECK_SIGNATURE")
    | C_HASH_KEY           , _   -> Some ( simple_unary @@ prim "HASH_KEY")
    | C_BYTES_PACK         , _   -> Some ( simple_unary @@ prim "PACK")
    | C_CONCAT             , _   -> Some ( simple_binary @@ prim "CONCAT")
    | C_CHAIN_ID           , _   -> Some ( simple_constant @@ prim "CHAIN_ID")
    | C_SHA3               , _   -> Some ( simple_unary @@ prim "SHA3")
    | C_KECCAK             , _   -> Some ( simple_unary @@ prim "KECCAK")
    | C_LEVEL              , _   -> Some ( simple_constant @@ prim "LEVEL")
    | C_VOTING_POWER       , _   -> Some ( simple_unary @@ prim "VOTING_POWER")
    | C_TOTAL_VOTING_POWER , _   -> Some ( simple_unary @@ prim "TOTAL_VOTING_POWER")

    | C_SELF               , _   -> Some (trivial_special "SELF")
    | C_NONE               , _   -> Some (trivial_special "NONE")
    | C_NIL                , _   -> Some (trivial_special "NIL")
    | C_LOOP_CONTINUE      , _   -> Some (trivial_special "LEFT")
    | C_LOOP_STOP          , _   -> Some (trivial_special "RIGHT")
    | C_LIST_EMPTY         , _   -> Some (trivial_special "NIL")
    | C_LIST_HEAD_OPT      , _   -> Some ( special @@ fun with_args -> i_if_cons (seq [i_swap; i_drop; i_some]) (with_args "NONE") )
    | C_LIST_TAIL_OPT      , _   -> Some ( special @@ fun with_args -> i_if_cons (seq [i_drop; i_some])       (with_args "NONE") )
    | C_SET_EMPTY          , _   -> Some (trivial_special "EMPTY_SET")
    | C_MAP_EMPTY          , _   -> Some (trivial_special "EMPTY_MAP")
    | C_BIG_MAP_EMPTY      , _   -> Some (trivial_special "EMPTY_BIG_MAP")
    | C_BYTES_UNPACK       , _   -> Some (trivial_special "UNPACK")
    | C_MAP_REMOVE         , _   -> Some (special (fun with_args -> seq [dip (with_args "NONE"); prim "UPDATE"]))
    | C_LEFT               , _   -> Some (trivial_special "LEFT")
    | C_RIGHT              , _   -> Some (trivial_special "RIGHT")
    | C_TICKET             , _ -> Some ( simple_binary @@ prim "TICKET" )
    | C_READ_TICKET        , _ -> Some ( simple_unary @@ seq [ prim "READ_TICKET" ; prim "PAIR" ] )
    | C_SPLIT_TICKET       , _ -> Some ( simple_binary @@ prim "SPLIT_TICKET" )
    | C_JOIN_TICKET        , _ -> Some ( simple_unary @@ prim "JOIN_TICKETS" )
    | C_SAPLING_EMPTY_STATE, _ -> Some (trivial_special "SAPLING_EMPTY_STATE")
    | C_SAPLING_VERIFY_UPDATE , _ -> Some (simple_binary @@ prim "SAPLING_VERIFY_UPDATE")
    | C_PAIRING_CHECK , _ -> Some (simple_binary @@ prim "PAIRING_CHECK")
    | C_CONTRACT           , _   ->
      Some (special
              (fun with_args ->
                 seq [with_args "CONTRACT";
                      i_assert_some_msg (i_push_string "bad address for get_contract")]))
    | C_CONTRACT_WITH_ERROR, _   ->
      Some (special
              (fun with_args ->
                 seq [with_args "CONTRACT";
                      i_if_none (i_failwith) (seq [i_swap; i_drop])]))
    | C_CONTRACT_OPT         , _   -> Some (trivial_special "CONTRACT")
    | C_CONTRACT_ENTRYPOINT , _  ->
      Some (special
              (fun with_args ->
                 seq [with_args "CONTRACT";
                      i_assert_some_msg (i_push_string "bad address for get_entrypoint")]))
    | C_CONTRACT_ENTRYPOINT_OPT , _ -> Some (trivial_special "CONTRACT")
    | C_CREATE_CONTRACT , _ ->
      Some (special
              (fun with_args ->
                 seq [with_args "CREATE_CONTRACT";
                      i_pair]))
    | C_OPEN_CHEST , Hangzhou -> (
      Some (simple_ternary @@ seq [
        prim "OPEN_CHEST" ;
        i_if_left
          ( prim "RIGHT" ~children:[t_or t_unit t_unit])
          ( i_if
            (seq [ i_push_unit ; prim "LEFT" ~children:[t_unit] ; prim "LEFT" ~children:[t_bytes] ])
            (seq [ i_push_unit ; prim "RIGHT" ~children:[t_unit] ; prim "LEFT" ~children:[t_bytes] ])
          ) 
      ])
    )
    | C_VIEW , Hangzhou -> Some (trivial_special "VIEW")
    | _ -> None

end
