open Helpers

module type S = sig
  module Context : Context.CTX

  module State = Context.State

  module Contract : sig
    type t [@@deriving yojson, eq]

    val raw_storage : t -> bytes
  end

  module Origination_payload : sig
    type t [@@deriving yojson]

    val lambda_of_yojson :
      code:Yojson.Safe.t -> storage:Yojson.Safe.t -> (t, string) result

    val dummy_of_yojson : storage:int -> t

    val wasm_of_yojson : code:bytes -> storage:bytes -> (t, string) result
  end

  module Compiler : sig
    val compile :
      Origination_payload.t -> gas:int -> (Contract.t, string) result
  end

  module Invocation_payload : sig
    type t [@@deriving yojson]

    val lambda_of_yojson : arg:Yojson.Safe.t -> (t, string) result

    val dummy_of_yojson : arg:Yojson.Safe.t -> (t, string) result

    val wasm_of_yojson : arg:Yojson.Safe.t -> (t, string) result

    val of_bytes : arg:bytes -> (t, string) result
  end

  module Interpreter : sig
    val invoke :
      Contract.t ->
      ctx:< State.table_access ; State.addressing ; State.with_operations ; .. > ->
      arg:Invocation_payload.t ->
      gas:int ->
      (* TODO: unit should be user operation list *)
      (Contract.t * int list, string) result
  end
end

open Core

module Make (CTX : Context.CTX) : S with module Context = CTX = struct
  module Context = CTX
  module State = Context.State

  module FFI = struct
    open Stdlib
    open CTX

    type syscall =
      | SENDER
      | SOURCE
      | SELF
      | OWN_TICKET
      | READ_TICKET
      | JOIN_TICKETS
      | SPLIT_TICKET
      | GET_CONTRACT_OPT
      | TRANSACTION

    open struct
      open Wasm_vm

      let int32_offset = 5

      let int64_offset = 9

      module W = struct
        let write_address ~writer_addr addr memory =
          let bytes = Address.to_bytes addr in
          Memory.store_bytes memory ~address:writer_addr ~content:bytes

        let write_ticket ~writer_addr ticket memory =
          let content = Ticket_handle.to_bytes ticket in
          Memory.store_bytes memory ~address:writer_addr ~content

        let write_tickets ~writer_addr tickets memory =
          let content =
            List.map Ticket_handle.to_bytes tickets |> Bytes.concat Bytes.empty
          in
          Memory.store_bytes memory ~address:writer_addr ~content

        let write_some ~writer_addr item writer memory =
          let i32 t =
            let b = Bytes.make 4 '0' in
            let () = Bytes.set_int32_le b 0 (Int32.of_int t) in
            b in
          match item with
          | None ->
            Memory.store_bytes ~address:writer_addr memory ~content:(i32 0)
          | Some item ->
            Memory.store_bytes ~address:writer_addr memory ~content:(i32 1);
            writer
              ~writer_addr:(Int64.add writer_addr (Int64.of_int int32_offset))
              item memory
      end

      module R = struct
        let read_ticket memory addr offs =
          let new_addr = Int64.add addr (offs |> Int64.of_int) in
          let addr =
            Bytes.get_int32_le
              (Memory.load_bytes memory ~address:new_addr ~size:4)
              0
            |> Int64.of_int32 in
          let ticket_handle =
            Memory.load_bytes memory ~address:addr ~size:Ticket_handle.size
          in
          let ticket_handle = Ticket_handle.of_bytes ticket_handle in
          (new_addr, ticket_handle)

        let read_amount memory addr offs =
          let new_addr = Int64.add addr (offs |> Int64.of_int) in
          let res =
            Bytes.get_int64_le
              (Memory.load_bytes memory ~address:new_addr ~size:Amount.size)
              0
            |> Int64.to_int
            |> Amount.of_int in
          (new_addr, res)

        let read_address memory addr offs =
          let new_addr = Int64.add addr (offs |> Int64.of_int) in
          let addr =
            Bytes.get_int32_le
              (Memory.load_bytes ~address:new_addr ~size:4 memory)
              0 in
          let res =
            Memory.load_bytes ~address:(Int64.of_int32 addr) ~size:Address.size
              memory
            |> Address.of_bytes in
          (new_addr, res)
      end
    end

    open Wasm_vm
    open CTX

    let load ~ctx memory writer_addr syscall =
      match syscall with
      | SENDER -> W.write_address ~writer_addr (Addressing.sender ctx) memory
      | SOURCE -> W.write_address ~writer_addr (Addressing.source ctx) memory
      | SELF -> W.write_address ~writer_addr (Addressing.self ctx) memory
      | READ_TICKET ->
        let _, ticket_handle = R.read_ticket memory writer_addr int32_offset in
        let store (ticket, amount, ticket_handle) =
          let buf = Bytes.make 8 '\x00' in
          let amount = Amount.to_int amount |> Int64.of_int in
          Bytes.set_int64_le buf 0 amount;
          let ticket = Ticket_id.to_bytes ticket in
          let ticket_handle = Ticket_handle.to_bytes ticket_handle in
          let content = Bytes.concat Bytes.empty [ticket; buf; ticket_handle] in
          Memory.store_bytes memory ~address:writer_addr ~content in
        store (Table_ops.read_ticket ctx ticket_handle)
      | SPLIT_TICKET ->
        let addr, ticket_handle =
          R.read_ticket memory writer_addr int32_offset in
        let addr, amount1 = R.read_amount memory addr int32_offset in
        let _, amount2 = R.read_amount memory addr int64_offset in
        let data = (ticket_handle, amount1, amount2) in
        let store (handle1, handle2) =
          W.write_tickets ~writer_addr [handle1; handle2] memory in
        store (Table_ops.split_ticket ctx data)
      | OWN_TICKET ->
        let _, ticket_handle = R.read_ticket memory writer_addr int32_offset in
        let store handle1 = W.write_ticket ~writer_addr handle1 memory in
        store (Table_ops.own_ticket ctx ticket_handle)
      | JOIN_TICKETS ->
        let addr, ticket_handle =
          R.read_ticket memory writer_addr int32_offset in
        let _, ticket_handle2 = R.read_ticket memory addr int32_offset in
        let store handle1 = W.write_ticket ~writer_addr handle1 memory in
        store (Table_ops.join_tickets ctx (ticket_handle, ticket_handle2))
      | TRANSACTION ->
        let addr =
          Bytes.get_int32_le
            (Memory.load_bytes memory
               ~address:(Int64.add writer_addr (int32_offset |> Int64.of_int))
               ~size:4)
            0
          |> Int64.of_int32 in
        let arg =
          if addr = -1L then
            Bytes.empty
          else
            let size =
              Bytes.get_int64_le
                (Memory.load_bytes memory ~address:addr ~size:8)
                0
              |> Int64.to_int in
            let bytes =
              Memory.load_bytes memory ~address:(Int64.add addr 8L) ~size in
            bytes in
        let addr, ticket_handle =
          R.read_ticket memory writer_addr (int32_offset * 2) in
        let addr, amount = R.read_amount memory addr int32_offset in
        let _, destination = R.read_address memory addr int64_offset in
        let store int =
          let buf = Bytes.make 4 '\x00' in
          Bytes.set_int32_le buf 0 (Int32.of_int int);
          Memory.store_bytes memory ~address:writer_addr ~content:buf in
        let called =
          Operations.transaction ctx (arg, (ticket_handle, amount), destination)
        in
        store called
      | GET_CONTRACT_OPT ->
        let _, address = R.read_address memory writer_addr int32_offset in
        let store addr = W.write_some ~writer_addr addr W.write_address memory in
        store (Addressing.get_contract_opt ctx address)

    let custom ~ctx mem x =
      match
        Bytes.get_int32_le (Memory.load_bytes mem ~address:x ~size:4) 0
        |> Int32.to_int
      with
      | 0 -> load ~ctx mem x SELF
      | 1 -> load ~ctx mem x SOURCE
      | 2 -> load ~ctx mem x SENDER
      | 3 -> load ~ctx mem x READ_TICKET
      | 4 -> load ~ctx mem x SPLIT_TICKET
      | 5 -> load ~ctx mem x OWN_TICKET
      | 6 -> load ~ctx mem x JOIN_TICKETS
      | 7 -> load ~ctx mem x GET_CONTRACT_OPT
      | 8 -> load ~ctx mem x TRANSACTION
      | _ -> failwith "unimplemented"
  end

  open CTX

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
          ~f:(function
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
          Raw_repr.Script.to_code ~gas code
          |> Result.map_error ~f:error_to_string in
        let%ok storage =
          Raw_repr.Value.to_value ~gas storage
          |> Result.map_error ~f:error_to_string in
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
          |> Result.map_error ~f:compiler_error_to_string in
        let arg = Ir.Value_syntax.pair argument storage in
        (* TODO: use invoked operations for something *)
        let%ok invoked =
          let sender = source |> Address.to_string in
          let context = Context.make ~sender ~source:sender gas in
          Interpreter.execute ~context ~arg contract.Contract.code
          |> Result.map_error ~f:error_to_string in
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
        Result.map ~f:(fun (c, ops) -> (c, ops)) result
    end
  end

  module Wasm = struct
    module Contract = struct
      type t = {
        code : Wasm_vm.Module.t;
        storage : bytes;
      }
      [@@deriving yojson]

      (* TODO: this is bad *)
      let equal a b = Poly.( = ) a b
    end

    module Origination_payload = struct
      type t = {
        code : bytes;
        storage : bytes;
      }
      [@@deriving yojson]

      let make ~code ~storage = { code; storage }
    end

    module Compiler = struct
      let compile ~gas:_ (code : Origination_payload.t) =
        let%ok compiled =
          Wasm_vm.Module.of_string ~code:(Bytes.to_string code.code) in
        Ok Contract.{ code = compiled; storage = code.storage }
    end

    module Invocation_payload = struct
      type t = bytes [@@deriving yojson]
    end

    module Interpreter = struct
      let invoke ctx contract arg ~gas =
        let Contract.{ code; storage } = contract in
        let custom = FFI.custom ~ctx in
        let gas = ref gas in
        let%ok updated, operations =
          Wasm_vm.Runtime.invoke custom ~module_:code ~storage ~gas
            ~argument:arg in
        let contract = Contract.{ code; storage = updated } in
        Ok (contract, operations)
    end
  end

  module Contract = struct
    type t =
      | Lambda of Lambda.Contract.t
      | Dummy  of Dummy.Contract.t
      | Wasm   of Wasm.Contract.t
    [@@deriving yojson, eq]

    let raw_storage t =
      match t with
      | Wasm c -> c.storage
      | _ -> Bytes.create 0
  end

  module Origination_payload = struct
    type t =
      | Dummy  of int
      | Lambda of Lambda.Origination_payload.t
      | Wasm   of Wasm.Origination_payload.t
    [@@deriving yojson]

    let lambda_of_yojson ~code ~storage =
      let%ok code = Lambda.Raw_repr.Script.of_yojson code in
      let%ok storage = Lambda.Raw_repr.Value.of_yojson storage in
      Ok (Lambda { code; storage })

    let dummy_of_yojson ~storage = Dummy storage

    let wasm_of_yojson ~code ~storage =
      Ok (Wasm (Wasm.Origination_payload.make ~code ~storage))
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
      | Wasm contract ->
        let%ok contract =
          Wasm.Compiler.compile contract ~gas
          |> Result.map_error ~f:Wasm_vm.Errors.show in
        Ok (Contract.Wasm contract)
  end

  module Invocation_payload = struct
    type t =
      | Lambda of Lambda.Invocation_payload.t
      | Dummy  of (int * int)
      | Wasm   of Wasm.Invocation_payload.t
    [@@deriving yojson]

    let lambda_of_yojson ~arg =
      let%ok arg = Lambda.Invocation_payload.of_yojson arg in
      Ok (Lambda arg)

    let dummy_of_yojson ~arg =
      let%ok arg = Dummy.Invocation_payload.of_yojson arg in
      Ok (Dummy arg)

    let wasm_of_yojson ~arg =
      let%ok arg = Wasm.Invocation_payload.of_yojson arg in
      Ok (Wasm arg)

    let of_bytes ~arg = Ok (Wasm arg)
  end

  module Interpreter = struct
    let invoke code ~ctx ~arg ~gas =
      match (code, arg) with
      | Contract.Dummy contract, Invocation_payload.Dummy arg ->
        let%ok contract, _ = Dummy.Interpreter.invoke contract ~arg ~gas in
        Ok (Contract.Dummy contract, [])
      | Lambda contract, Invocation_payload.Lambda arg ->
        let%ok contract, _ =
          Lambda.Interpreter.invoke contract ~source:ctx#source ~arg ~gas in
        Ok (Contract.Lambda contract, [])
      | Wasm contract, Invocation_payload.Wasm arg ->
        let%ok contract, operations =
          Wasm.Interpreter.invoke ctx contract arg ~gas
          |> Result.map_error ~f:Wasm_vm.Errors.show in
        Ok (Contract.Wasm contract, operations)
      | _, _ -> Error "Invocation failure"
  end
end
