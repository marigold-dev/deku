open Stage_common.Enums

let constant'_tag = function
  | C_INT                     ->   1
  | C_UNIT                    ->   2
  | C_NIL                     ->   3
  | C_NOW                     ->   4
  | C_IS_NAT                  ->   5
  | C_SOME                    ->   6
  | C_NONE                    ->   7
  | C_UNOPT                   ->   8
  | C_UNOPT_WITH_ERROR        -> 800
  | C_ASSERTION               ->   9
  | C_ASSERTION_WITH_ERROR    ->  10
  | C_ASSERT_INFERRED         ->  11
  | C_ASSERT_SOME             ->  12
  | C_ASSERT_SOME_WITH_ERROR  ->  13
  | C_FAILWITH                ->  14
  | C_UPDATE                  ->  15
  (* Loops *)
  | C_ITER                    ->  16
  | C_FOLD_WHILE              ->  17
  | C_FOLD_CONTINUE           ->  18
  | C_FOLD_STOP               ->  19
  | C_LOOP_LEFT               ->  20
  | C_LOOP_CONTINUE           ->  21
  | C_LOOP_STOP               ->  22
  | C_FOLD                    ->  23
  | C_FOLD_LEFT               ->  24
  | C_FOLD_RIGHT              ->  25
  (* MATH *)
  | C_NEG                     ->  26
  | C_ABS                     ->  27
  | C_ADD                     ->  28
  | C_SUB                     ->  29
  | C_MUL                     ->  30
  | C_EDIV                    ->  31
  | C_DIV                     ->  32
  | C_MOD                     ->  33
  (* LOGIC *)
  | C_NOT                     ->  34
  | C_AND                     ->  35
  | C_OR                      ->  36
  | C_XOR                     ->  37
  | C_LSL                     ->  38
  | C_LSR                     ->  39
  (* COMPARATOR *)
  | C_EQ                      ->  40
  | C_NEQ                     ->  41
  | C_LT                      ->  42
  | C_GT                      ->  43
  | C_LE                      ->  44
  | C_GE                      ->  45
  (* Bytes/ String *)
  | C_SIZE                    ->  46
  | C_CONCAT                  ->  47
  | C_SLICE                   ->  48
  | C_BYTES_PACK              ->  49
  | C_BYTES_UNPACK            ->  50
  | C_CONS                    ->  51
  (* Pair *)
  | C_PAIR                    ->  52
  | C_CAR                     ->  53
  | C_CDR                     ->  54
  | C_LEFT                    ->  55
  | C_RIGHT                   ->  56
  (* Set *)
  | C_SET_EMPTY               ->  57
  | C_SET_LITERAL             ->  58
  | C_SET_ADD                 ->  59
  | C_SET_REMOVE              ->  60
  | C_SET_UPDATE              ->  61
  | C_SET_ITER                ->  62
  | C_SET_FOLD                ->  63
  | C_SET_FOLD_DESC           ->  64
  | C_SET_MEM                 ->  65
  (* List *)
  | C_LIST_EMPTY              ->  66
  | C_LIST_LITERAL            ->  67
  | C_LIST_ITER               ->  68
  | C_LIST_MAP                ->  69
  | C_LIST_FOLD               ->  70
  | C_LIST_FOLD_LEFT          ->  71
  | C_LIST_FOLD_RIGHT         ->  72
  | C_LIST_HEAD_OPT           ->  73
  | C_LIST_TAIL_OPT           ->  74
  (* Maps *)
  | C_MAP                     ->  75
  | C_MAP_EMPTY               ->  76
  | C_MAP_LITERAL             ->  77
  | C_MAP_GET                 ->  78
  | C_MAP_GET_FORCE           ->  79
  | C_MAP_ADD                 ->  80
  | C_MAP_REMOVE              ->  81
  | C_MAP_UPDATE              ->  82
  | C_MAP_ITER                ->  83
  | C_MAP_MAP                 ->  84
  | C_MAP_FOLD                ->  85
  | C_MAP_MEM                 ->  86
  | C_MAP_FIND                ->  87
  | C_MAP_FIND_OPT            ->  88
  (* Big Maps *)
  | C_BIG_MAP                 ->  89
  | C_BIG_MAP_EMPTY           ->  90
  | C_BIG_MAP_LITERAL         ->  91
  (* Crypto *)
  | C_SHA256                  ->  92
  | C_SHA512                  ->  93
  | C_BLAKE2b                 ->  94
  | C_HASH                    ->  95
  | C_HASH_KEY                ->  96
  | C_CHECK_SIGNATURE         ->  97
  | C_CHAIN_ID                ->  98
  (* Blockchain *)
  | C_CALL                    ->  99
  | C_CONTRACT                -> 100
  | C_CONTRACT_WITH_ERROR     -> 101
  | C_CONTRACT_OPT            -> 102
  | C_CONTRACT_ENTRYPOINT     -> 103
  | C_CONTRACT_ENTRYPOINT_OPT -> 104
  | C_AMOUNT                  -> 105
  | C_BALANCE                 -> 106
  | C_SOURCE                  -> 107
  | C_SENDER                  -> 108
  | C_ADDRESS                 -> 109
  | C_SELF                    -> 110
  | C_SELF_ADDRESS            -> 111
  | C_IMPLICIT_ACCOUNT        -> 112
  | C_SET_DELEGATE            -> 113
  | C_CREATE_CONTRACT         -> 114
  | C_TRUE                    -> 119
  | C_FALSE                   -> 120
  | C_TEST_ORIGINATE          -> 121
  | C_TEST_SET_NOW            -> 122
  | C_TEST_SET_SOURCE         -> 123
  | C_TEST_SET_BAKER          -> 124
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT -> 125
  | C_TEST_GET_STORAGE        -> 126
  | C_TEST_GET_BALANCE        -> 127
  | C_TEST_MICHELSON_EQUAL    -> 128
  | C_TEST_LOG                -> 129
  | C_SHA3                    -> 130
  | C_KECCAK                  -> 131
  | C_LEVEL                   -> 132
  | C_VOTING_POWER            -> 133
  | C_TOTAL_VOTING_POWER      -> 134
  | C_TICKET                  -> 135
  | C_READ_TICKET             -> 136
  | C_SPLIT_TICKET            -> 137
  | C_JOIN_TICKET             -> 138
  | C_PAIRING_CHECK           -> 139
  | C_MAP_GET_AND_UPDATE      -> 140
  | C_BIG_MAP_GET_AND_UPDATE  -> 141
  | C_SAPLING_EMPTY_STATE     -> 142
  | C_SAPLING_VERIFY_UPDATE   -> 143
  | C_TEST_STATE_RESET        -> 144
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN  -> 145
  | C_TEST_GET_NTH_BS         -> 146
  | C_TEST_LAST_ORIGINATIONS  -> 147
  | C_TEST_COMPILE_META_VALUE -> 148
  | C_TEST_RUN                -> 150
  | C_TEST_EVAL               -> 151
  | C_TEST_COMPILE_CONTRACT   -> 152
  | C_TEST_TO_CONTRACT        -> 153
  | C_TEST_ORIGINATE_FROM_FILE -> 154
  | C_TEST_GET_STORAGE_OF_ADDRESS -> 155
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS -> 156
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN -> 157
  | C_TEST_TO_ENTRYPOINT        -> 158
  | C_POLYMORPHIC_ADD         -> 159
  | C_NEVER                   -> 161
  | C_TEST_MUTATE_COUNT       -> 162
  | C_TEST_MUTATE_VALUE       -> 163
  | C_TEST_MUTATION_TEST      -> 164
  | C_TEST_MUTATION_TEST_ALL  -> 165
  | C_TEST_BOOTSTRAP_CONTRACT -> 166
  | C_TEST_NTH_BOOTSTRAP_CONTRACT -> 167
  | C_TEST_TO_TYPED_ADDRESS   -> 168
  | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS -> 169
  | C_TEST_SAVE_MUTATION      -> 170
  | C_TEST_SET_BIG_MAP        -> 171
  | C_OPEN_CHEST -> 172
  | C_VIEW -> 173
  | C_TEST_CAST_ADDRESS -> 174
  | C_TEST_CREATE_CHEST -> 175
  | C_TEST_CREATE_CHEST_KEY -> 176

let constant' a b = Int.compare (constant'_tag a) (constant'_tag b)

let literal_tag = function
  | Literal_unit        ->  1
  | Literal_int _       ->  2
  | Literal_nat _       ->  3
  | Literal_timestamp _ ->  4
  | Literal_mutez _     ->  5
  | Literal_string _    ->  6
  | Literal_bytes _     ->  7
  | Literal_address _   ->  8
  | Literal_signature _ ->  9
  | Literal_key _       -> 10
  | Literal_key_hash _  -> 11
  | Literal_chain_id _  -> 12
  | Literal_operation _ -> 13

let literal a b = Int.compare (literal_tag a) (literal_tag b)
