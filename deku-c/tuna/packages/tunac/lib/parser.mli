module MPrim = Michelson_primitives

val parse_expr :
     string
  -> ( MPrim.prim Tezos_micheline.Micheline.canonical
     , [ `Parsing_error of Tezos_error_monad.Error_monad.tztrace
       | `Prim_parsing_error of MPrim.error
       ] )
     result
