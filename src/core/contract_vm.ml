open Helpers

module type VM = sig
  module Contract : sig
    type t [@@deriving yojson, eq]
  end
  module Origination_payload : sig
    type t [@@deriving yojson]
    val lambda_of_yojson :
      code:Yojson.Safe.t -> storage:Yojson.Safe.t -> (t, string) result
  end

  module Compiler : sig
    val compile :
      Origination_payload.t -> gas:int -> (Contract.t, string) result
  end
end

module Lambda : VM = struct
  module Contract = struct
    type code = Lambda_vm.Ir.script [@@deriving yojson, eq]
    type value = Lambda_vm.Ir.value [@@deriving yojson, eq]

    type t = {
      code : code;
      storage : value;
    }
    [@@deriving yojson, eq]

    let make ~code ~storage = { code; storage }
  end

  module Raw_repr = struct
    open Lambda_vm
    module Errors = struct
      type t =
        [ `Out_of_gas
        | `Out_of_stack
        | `Invalid_contract
        | `Invalid_contract_argument ]
    end
    let wrap_error x =
      Result.map_error
        (function
          (* TODO: use this compiler_error properly *)
          | Compiler.Compiler_error _compiler_error -> `Invalid_contract
          | Runtime_limits_error Out_of_gas -> `Out_of_gas
          | Runtime_limits_error Out_of_stack -> `Out_of_stack)
        x

    module Script = struct
      type t = Ast.script [@@deriving yojson]
      let to_code ~gas script = wrap_error (Compiler.compile gas script)
    end

    (** TODO: conversion from micheline *)
    module Value = struct
      type t = Ast.value [@@deriving yojson]
      let to_value ~gas value = wrap_error (Compiler.compile_value gas value)
    end
  end

  module Origination_payload = struct
    type t = {
      code : Raw_repr.Script.t;
      storage : Raw_repr.Value.t;
    }
    [@@deriving yojson]

    let lambda_of_yojson ~code ~storage =
      let%ok code = Raw_repr.Script.of_yojson code in
      let%ok storage = Raw_repr.Value.of_yojson storage in
      Ok { code; storage }
  end
  module Compiler = struct
    let error_to_string : Raw_repr.Errors.t -> string = function
      | `Out_of_gas -> "Out of gas"
      | `Out_of_stack -> "Out of stack"
      | `Invalid_contract -> "Invalid contract"
      | `Invalid_contract_argument -> "Invalid contract argument"
    let compile ({ code; storage } : Origination_payload.t) ~gas =
      let open Lambda_vm in
      let gas = Gas.make ~initial_gas:gas in
      let%ok code =
        Raw_repr.Script.to_code ~gas code |> Result.map_error error_to_string
      in
      let%ok storage =
        Raw_repr.Value.to_value ~gas storage |> Result.map_error error_to_string
      in
      Ok (Contract.make ~code ~storage)
  end
end

include Lambda
