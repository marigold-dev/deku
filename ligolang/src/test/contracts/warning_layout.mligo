type parameter_warns =
  [@layout:comb]
    B of nat
  | C of int
  | D of string
  | A of unit

type parameter_ok =
  [@layout:comb]
  | B of nat
  | C of int
  | D of string
  | A of unit
