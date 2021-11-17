open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "protocol_dalphanet.mligo" ] ;
  [%expect{|
    File "../../test/contracts/protocol_dalphanet.mligo", line 12, characters 13-14:
     11 |
     12 | let main (p, s : bls_l * bool) : operation list * bool =
     13 |  (([] : operation list), Tezos.pairing_check p)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter (list (pair bls12_381_g1 bls12_381_g2)) ;
      storage bool ;
      code { CAR ; PAIRING_CHECK ; NIL operation ; PAIR } } |}] ;

  run_ligo_good [ "print" ; "ast-typed" ; contract "protocol_dalphanet.mligo" ; "--protocol"; "edo" ; ] ;
  [%expect {xxx|
    type bls_l = list (( bls12_381_g1 * bls12_381_g2 ))
    const a = [%Michelson {|
      { PUSH @vk_gamma_c bls12_381_g1 0x063bd6e11e2fcaac1dd8cf68c6b1925a73c3c583e298ed37c41c3715115cf96358a42dbe85a0228cbfd8a6c8a8c54cd015b5ae2860d1cc47f84698d951f14d9448d03f04df2ca0ffe609a2067d6f1a892163a5e05e541279134cae52b1f23c6b; }
      |}]
    const b = [%Michelson {|
        { PUSH @vk_delta bls12_381_g2 0x10c6d5cdca84fc3c7f33061add256f48e0ab03a697832b338901898b650419eb6f334b28153fb73ad2ecd1cd2ac67053161e9f46cfbdaf7b1132a4654a55162850249650f9b873ac3113fa8c02ef1cd1df481480a4457f351d28f4da89d19fa405c3d77f686dc9a24d2681c9184bf2b091f62e6b24df651a3da8bd7067e14e7908fb02f8955b84af5081614cb5bc49b416d9edf914fc608c441b3f2eb8b6043736ddb9d4e4d62334a23b5625c14ef3e1a7e99258386310221b22d83a5eac035c; }
      |}]
    const main = lambda (#1) return let #3 = #1 in  match #3 with
                                                     | ( p , s ) ->
                                                     ( LIST_EMPTY() , PAIRING_CHECK(p) ) |xxx}] 

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "sapling.mligo" ; "--disable-michelson-typechecking" ; "--protocol"; "edo" ] ;
  [%expect {|
    File "../../test/contracts/sapling.mligo", line 8, characters 14-19:
      7 |
      8 | let main (tr, store : parameter * storage) : return =
      9 |  ([] : operation list),
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    { parameter (sapling_transaction 8) ;
      storage (pair int (sapling_state 8)) ;
      code { CAR ;
             SAPLING_EMPTY_STATE 8 ;
             SWAP ;
             SAPLING_VERIFY_UPDATE ;
             IF_NONE { PUSH string "failed" ; FAILWITH } {} ;
             NIL operation ;
             PAIR } } |}] ;
