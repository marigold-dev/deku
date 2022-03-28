open Helpers
open Crypto

module Raw = struct
  module M = struct
    type t = Yojson.Safe.t [@@deriving yojson]
    let make t =
      try Ok (Yojson.Safe.from_string t) with
      | Yojson.Json_error _ -> Error "invalid input"
  end
  module Script = M
  module Value = M
end
module type VM = sig
  module Contract : sig
    type t [@@deriving yojson]
    val originated_by : t -> Key_hash.t
    val to_string : t -> string
  end
  module Origination_payload : sig
    type t [@@deriving yojson]
    val pp : Format.formatter -> t -> unit
    val make : code:Raw.Script.t -> storage:Raw.Value.t -> (t, string) result
  end

  module Compile : sig
    val compile :
      Origination_payload.t ->
      gas:int ->
      originated_by:Key_hash.t ->
      on_error:(string -> int -> 'error) ->
      (Contract.t * int, 'error) result
  end
end
module Lambda : VM = struct
  module Contract = struct
    type code = Lambda_vm.Ir.script [@@deriving yojson]
    type value = Lambda_vm.Ir.value [@@deriving yojson]

    type t = {
      code : code;
      storage : value;
      originated_by : Key_hash.t;
    }
    [@@deriving yojson]

    let make ~code ~storage ~originator =
      { code; storage; originated_by = originator }

    let to_string t = to_yojson t |> Yojson.Safe.pretty_to_string
    let originated_by t = t.originated_by
  end

  module Raw = struct
    open Lambda_vm
    module Errors = struct
      type t =
        [ `Out_of_gas
        | `Out_of_stack
        | `Invalid_contract
        | `Invalid_contract_argument ]
    end
    module Script = struct
      type t = Ast.script [@@deriving yojson, show]
      let to_code ~gas script =
        Compiler.compile gas script
        |> Result.map_error (function
             | `Undefined_variable -> `Invalid_contract
             | (`Out_of_gas | `Out_of_stack) as x -> x)
    end
    module Value = struct
      type t = Ast.value [@@deriving yojson, show]
      let to_value ~gas value =
        Compiler.compile_value gas value
        |> Result.map_error (function
             | `Undefined_variable -> `Invalid_contract_argument
             | (`Out_of_gas | `Out_of_stack) as x -> x)
    end
  end

  module Origination_payload = struct
    type t = {
      code : Raw.Script.t;
      storage : Raw.Value.t;
    }
    [@@deriving yojson, show]
    let handle_failure t ~msg = Result.map_error (fun _ -> msg) t

    let make ~code ~storage =
      let%ok code =
        Raw.Script.of_yojson code
        |> handle_failure ~msg:"failed to parse the contract" in
      let%ok storage =
        Raw.Value.of_yojson storage
        |> handle_failure ~msg:"failed to parse the storage" in
      Ok { code; storage }
  end
  module Compile = struct
    let error_to_string : Raw.Errors.t -> string = function
      | `Out_of_gas -> "Out of gas"
      | `Out_of_stack -> "Out of stack"
      | `Invalid_contract -> "Invalid contract"
      | `Invalid_contract_argument -> "Invalid contract argument"
    let compile ({ code; storage } : Origination_payload.t) ~gas ~originated_by
        ~on_error =
      let open Lambda_vm in
      let initial_gas = gas in
      let gas = Gas.make ~initial_gas:gas in
      let%ok code =
        Raw.Script.to_code ~gas code
        |> Result.map_error (fun x -> on_error (error_to_string x) initial_gas)
      in
      let%ok storage =
        Raw.Value.to_value ~gas storage
        |> Result.map_error (fun x -> on_error (error_to_string x) initial_gas)
      in
      Ok
        ( Contract.make ~originator:originated_by ~code ~storage,
          initial_gas - Gas.to_int gas )
  end
end

module Origination_payload = struct
  type t =
    | Lambda of Lambda.Origination_payload.t
    | Dummy
  [@@deriving yojson, show]
  let make_lambda ~code ~storage : (t, string) result =
    (* ppx errors here for some reason *)
    let originated = Lambda.Origination_payload.make ~code ~storage in
    Result.map (fun x -> Lambda x) originated
end

module Contract = struct
  type t =
    | Lambda of Lambda.Contract.t
    | Dummy  of bytes
  [@@deriving yojson]
  let to_string = function
    | Lambda contract -> Lambda.Contract.to_string contract
    | Dummy _ -> failwith "unimplemented"
  let originated_by = function
    | Lambda contract -> Lambda.Contract.originated_by contract
    | Dummy _ -> failwith "unimplemented"
  module Compile = struct
    let compile_script t ~gas ~on_error ~originated_by =
      match t with
      | Origination_payload.Dummy -> failwith "Test"
      | Lambda contract ->
        let compiled =
          Lambda.Compile.compile ~gas ~originated_by ~on_error contract in
        Result.map (fun (compiled, gas) -> (Lambda compiled, gas)) compiled
  end
end

module Contract_storage = struct
  module M = Map.Make_with_yojson (Tezos.Contract_hash)

  type t = Contract.t M.t [@@deriving yojson]

  (* TODO: actual dummy vm *)
  let empty = M.empty

  let originate_contract t ~address ~contract = M.add address contract t

  let update_contract_storage t ~address ~updated_contract =
    M.update address (Option.map (fun _ -> updated_contract)) t

  let get_contract t ~address = M.find_opt address t
end
