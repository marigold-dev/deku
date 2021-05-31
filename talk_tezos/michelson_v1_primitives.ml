(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type prim =
  | K_parameter
  | K_storage
  | K_code
  | D_False
  | D_Elt
  | D_Left
  | D_None
  | D_Pair
  | D_Right
  | D_Some
  | D_True
  | D_Unit
  | I_PACK
  | I_UNPACK
  | I_BLAKE2B
  | I_SHA256
  | I_SHA512
  | I_ABS
  | I_ADD
  | I_AMOUNT
  | I_AND
  | I_BALANCE
  | I_CAR
  | I_CDR
  | I_CHAIN_ID
  | I_CHECK_SIGNATURE
  | I_COMPARE
  | I_CONCAT
  | I_CONS
  | I_CREATE_ACCOUNT
  | I_CREATE_CONTRACT
  | I_IMPLICIT_ACCOUNT
  | I_DIP
  | I_DROP
  | I_DUP
  | I_EDIV
  | I_EMPTY_BIG_MAP
  | I_EMPTY_MAP
  | I_EMPTY_SET
  | I_EQ
  | I_EXEC
  | I_APPLY
  | I_FAILWITH
  | I_GE
  | I_GET
  | I_GET_AND_UPDATE
  | I_GT
  | I_HASH_KEY
  | I_IF
  | I_IF_CONS
  | I_IF_LEFT
  | I_IF_NONE
  | I_INT
  | I_LAMBDA
  | I_LE
  | I_LEFT
  | I_LEVEL
  | I_LOOP
  | I_LSL
  | I_LSR
  | I_LT
  | I_MAP
  | I_MEM
  | I_MUL
  | I_NEG
  | I_NEQ
  | I_NIL
  | I_NONE
  | I_NOT
  | I_NOW
  | I_OR
  | I_PAIR
  | I_UNPAIR
  | I_PUSH
  | I_RIGHT
  | I_SIZE
  | I_SOME
  | I_SOURCE
  | I_SENDER
  | I_SELF
  | I_SELF_ADDRESS
  | I_SLICE
  | I_STEPS_TO_QUOTA
  | I_SUB
  | I_SWAP
  | I_TRANSFER_TOKENS
  | I_SET_DELEGATE
  | I_UNIT
  | I_UPDATE
  | I_XOR
  | I_ITER
  | I_LOOP_LEFT
  | I_ADDRESS
  | I_CONTRACT
  | I_ISNAT
  | I_CAST
  | I_RENAME
  | I_SAPLING_EMPTY_STATE
  | I_SAPLING_VERIFY_UPDATE
  | I_DIG
  | I_DUG
  | I_NEVER
  | I_VOTING_POWER
  | I_TOTAL_VOTING_POWER
  | I_KECCAK
  | I_SHA3
  | I_PAIRING_CHECK
  | I_TICKET
  | I_READ_TICKET
  | I_SPLIT_TICKET
  | I_JOIN_TICKETS
  | T_bool
  | T_contract
  | T_int
  | T_key
  | T_key_hash
  | T_lambda
  | T_list
  | T_map
  | T_big_map
  | T_nat
  | T_option
  | T_or
  | T_pair
  | T_set
  | T_signature
  | T_string
  | T_bytes
  | T_mutez
  | T_timestamp
  | T_unit
  | T_operation
  | T_address
  | T_sapling_transaction
  | T_sapling_state
  | T_chain_id
  | T_never
  | T_bls12_381_g1
  | T_bls12_381_g2
  | T_bls12_381_fr
  | T_ticket
let prim_encoding =
  let open Data_encoding in
  def "michelson.v1.primitives"
  @@ string_enum
       (* Add the comment below every 10 lines *)
       [ (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("parameter", K_parameter);
         ("storage", K_storage);
         ("code", K_code);
         ("False", D_False);
         ("Elt", D_Elt);
         ("Left", D_Left);
         ("None", D_None);
         ("Pair", D_Pair);
         ("Right", D_Right);
         ("Some", D_Some);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("True", D_True);
         ("Unit", D_Unit);
         ("PACK", I_PACK);
         ("UNPACK", I_UNPACK);
         ("BLAKE2B", I_BLAKE2B);
         ("SHA256", I_SHA256);
         ("SHA512", I_SHA512);
         ("ABS", I_ABS);
         ("ADD", I_ADD);
         ("AMOUNT", I_AMOUNT);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("AND", I_AND);
         ("BALANCE", I_BALANCE);
         ("CAR", I_CAR);
         ("CDR", I_CDR);
         ("CHECK_SIGNATURE", I_CHECK_SIGNATURE);
         ("COMPARE", I_COMPARE);
         ("CONCAT", I_CONCAT);
         ("CONS", I_CONS);
         ("CREATE_ACCOUNT", I_CREATE_ACCOUNT);
         ("CREATE_CONTRACT", I_CREATE_CONTRACT);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("IMPLICIT_ACCOUNT", I_IMPLICIT_ACCOUNT);
         ("DIP", I_DIP);
         ("DROP", I_DROP);
         ("DUP", I_DUP);
         ("EDIV", I_EDIV);
         ("EMPTY_MAP", I_EMPTY_MAP);
         ("EMPTY_SET", I_EMPTY_SET);
         ("EQ", I_EQ);
         ("EXEC", I_EXEC);
         ("FAILWITH", I_FAILWITH);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("GE", I_GE);
         ("GET", I_GET);
         ("GT", I_GT);
         ("HASH_KEY", I_HASH_KEY);
         ("IF", I_IF);
         ("IF_CONS", I_IF_CONS);
         ("IF_LEFT", I_IF_LEFT);
         ("IF_NONE", I_IF_NONE);
         ("INT", I_INT);
         ("LAMBDA", I_LAMBDA);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("LE", I_LE);
         ("LEFT", I_LEFT);
         ("LOOP", I_LOOP);
         ("LSL", I_LSL);
         ("LSR", I_LSR);
         ("LT", I_LT);
         ("MAP", I_MAP);
         ("MEM", I_MEM);
         ("MUL", I_MUL);
         ("NEG", I_NEG);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("NEQ", I_NEQ);
         ("NIL", I_NIL);
         ("NONE", I_NONE);
         ("NOT", I_NOT);
         ("NOW", I_NOW);
         ("OR", I_OR);
         ("PAIR", I_PAIR);
         ("PUSH", I_PUSH);
         ("RIGHT", I_RIGHT);
         ("SIZE", I_SIZE);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("SOME", I_SOME);
         ("SOURCE", I_SOURCE);
         ("SENDER", I_SENDER);
         ("SELF", I_SELF);
         ("STEPS_TO_QUOTA", I_STEPS_TO_QUOTA);
         ("SUB", I_SUB);
         ("SWAP", I_SWAP);
         ("TRANSFER_TOKENS", I_TRANSFER_TOKENS);
         ("SET_DELEGATE", I_SET_DELEGATE);
         ("UNIT", I_UNIT);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("UPDATE", I_UPDATE);
         ("XOR", I_XOR);
         ("ITER", I_ITER);
         ("LOOP_LEFT", I_LOOP_LEFT);
         ("ADDRESS", I_ADDRESS);
         ("CONTRACT", I_CONTRACT);
         ("ISNAT", I_ISNAT);
         ("CAST", I_CAST);
         ("RENAME", I_RENAME);
         ("bool", T_bool);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("contract", T_contract);
         ("int", T_int);
         ("key", T_key);
         ("key_hash", T_key_hash);
         ("lambda", T_lambda);
         ("list", T_list);
         ("map", T_map);
         ("big_map", T_big_map);
         ("nat", T_nat);
         ("option", T_option);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("or", T_or);
         ("pair", T_pair);
         ("set", T_set);
         ("signature", T_signature);
         ("string", T_string);
         ("bytes", T_bytes);
         ("mutez", T_mutez);
         ("timestamp", T_timestamp);
         ("unit", T_unit);
         ("operation", T_operation);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("address", T_address);
         (* Alpha_002 addition *)
         ("SLICE", I_SLICE);
         (* Alpha_005 addition *)
         ("DIG", I_DIG);
         ("DUG", I_DUG);
         ("EMPTY_BIG_MAP", I_EMPTY_BIG_MAP);
         ("APPLY", I_APPLY);
         ("chain_id", T_chain_id);
         ("CHAIN_ID", I_CHAIN_ID);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         (* Alpha_008 addition *)
         ("LEVEL", I_LEVEL);
         ("SELF_ADDRESS", I_SELF_ADDRESS);
         ("never", T_never);
         ("NEVER", I_NEVER);
         ("UNPAIR", I_UNPAIR);
         ("VOTING_POWER", I_VOTING_POWER);
         ("TOTAL_VOTING_POWER", I_TOTAL_VOTING_POWER);
         ("KECCAK", I_KECCAK);
         ("SHA3", I_SHA3);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         (* Alpha_008 addition *)
         ("PAIRING_CHECK", I_PAIRING_CHECK);
         ("bls12_381_g1", T_bls12_381_g1);
         ("bls12_381_g2", T_bls12_381_g2);
         ("bls12_381_fr", T_bls12_381_fr);
         ("sapling_state", T_sapling_state);
         ("sapling_transaction", T_sapling_transaction);
         ("SAPLING_EMPTY_STATE", I_SAPLING_EMPTY_STATE);
         ("SAPLING_VERIFY_UPDATE", I_SAPLING_VERIFY_UPDATE);
         ("ticket", T_ticket);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         (* Alpha_008 addition *)
         ("TICKET", I_TICKET);
         ("READ_TICKET", I_READ_TICKET);
         ("SPLIT_TICKET", I_SPLIT_TICKET);
         ("JOIN_TICKETS", I_JOIN_TICKETS);
         ("GET_AND_UPDATE", I_GET_AND_UPDATE)
         (* New instructions must be added here, for backward compatibility of the encoding. *)
         (* Keep the comment above at the end of the list *)
        ]