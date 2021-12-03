module Formatter = Formatter
module Types = Types

include Types

let typecheck_contract_tracer c errs : all =
  let open Tezos_micheline.Micheline in
  let c = root (strip_locations c) in
  main_typecheck_contract_tracer c errs
