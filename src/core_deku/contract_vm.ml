open Helpers

module Lambda = struct
  module Contract = struct
    type t = {
      code : Lambda_vm.Ir.code;
      storage : Lambda_vm.Ir.value;
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

  module Invocation_payload = struct
    type t = Raw_repr.Value.t [@@deriving yojson]
  end

  module Interpreter = struct
    let error_to_string : Lambda_vm.Interpreter.error -> string = function
      | Runtime_limits_error Out_of_gas -> "Out of gas"
      | Runtime_limits_error Out_of_stack -> "Out of stack"
      | Interpreter_error
          ( Value_is_not_pair | Value_is_not_function | Value_is_not_zero
          | Value_is_not_int64 ) ->
        "Invalid argument passed to the contract/Error within contract code"
      | Interpreter_error (Undefined_variable | Over_applied_primitives) ->
        "WOOOOOO!1!1! Bug within Lambda_vm interpreter"

    let invoke contract ~source ~(arg : Invocation_payload.t) ~gas =
      let compiler_error_to_string = Compiler.error_to_string in
      let open Lambda_vm in
      let storage = contract.Contract.storage in
      let gas = Gas.make ~initial_gas:gas in
      let%ok argument =
        Raw_repr.Value.to_value ~gas arg
        |> Result.map_error compiler_error_to_string in
      let arg = Ir.Value_syntax.pair argument storage in
      (* TODO: use invoked operations for something *)
      let%ok invoked =
        let sender = source |> Crypto.Key_hash.to_string in
        let context = Context.make ~sender ~source:sender gas in
        Interpreter.execute ~context ~arg contract.Contract.code
        |> Result.map_error error_to_string in
      let updated_contract =
        Contract.make ~code:contract.code ~storage:invoked.storage in
      Ok (updated_contract, ())
  end
end

module Dummy = struct
  module Contract = struct
    type t = int [@@deriving yojson, eq]
  end

  module Origination_payload = struct
    type t = int [@@deriving yojson]
  end

  module Compiler = struct
    let compile payload ~gas:_ = Ok payload
  end

  module Invocation_payload = struct
    type t = int * int [@@deriving yojson]
  end

  module Interpreter = struct
    let invoke storage ~arg ~gas:_ =
      let result =
        match arg with
        | 0, num -> Ok (storage + num, ())
        | 1, num -> Ok (storage - num, ())
        | _, _ -> Error "Invalid arg" in
      result |> Result.map (fun (c, ops) -> (c, ops))
  end
end

module External_vm = struct
  module Contract = struct
    type t =
      | Lambda of Lambda.Contract.t
      | Dummy  of Dummy.Contract.t
    [@@deriving yojson, eq]
  end

  module Origination_payload = struct
    type t =
      | Dummy  of int
      | Lambda of Lambda.Origination_payload.t
    [@@deriving yojson]

    let lambda_of_yojson ~code ~storage =
      let%ok code = Lambda.Raw_repr.Script.of_yojson code in
      let%ok storage = Lambda.Raw_repr.Value.of_yojson storage in
      Ok (Lambda { code; storage })

    let dummy_of_yojson ~storage = Dummy storage
  end

  module Compiler = struct
    let compile payload ~gas =
      match payload with
      | Origination_payload.Dummy contract ->
        let%ok contract = Dummy.Compiler.compile contract ~gas in
        Ok (Contract.Dummy contract)
      | Lambda contract ->
        let%ok contract = Lambda.Compiler.compile contract ~gas in
        Ok (Contract.Lambda contract)
  end

  module Invocation_payload = struct
    type t =
      | Lambda of Lambda.Invocation_payload.t
      | Dummy  of (int * int)
    [@@deriving yojson]

    let lambda_of_yojson ~arg =
      let%ok arg = Lambda.Invocation_payload.of_yojson arg in
      Ok (Lambda arg)

    let dummy_of_yojson ~arg =
      let%ok arg = Dummy.Invocation_payload.of_yojson arg in
      Ok (Dummy arg)
  end

  module Interpreter = struct
    let invoke code ~source ~arg ~gas =
      match (code, arg) with
      | Contract.Dummy contract, Invocation_payload.Dummy arg ->
        let%ok contract, operations =
          Dummy.Interpreter.invoke contract ~arg ~gas in
        Ok (Contract.Dummy contract, operations)
      | Lambda contract, Invocation_payload.Lambda arg ->
        let%ok contract, operations =
          Lambda.Interpreter.invoke contract ~source ~arg ~gas in
        Ok (Contract.Lambda contract, operations)
      | _, _ -> Error "Invocation failure"
  end
end

include External_vm
