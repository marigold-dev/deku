open Helpers
open Conversions
open Core
module State = Contract_ffi.State
module FFI = Wasm_ffi

module type S = sig
  module Contract : sig
    type t [@@deriving yojson, eq]

    val raw_storage : t -> bytes

    val update_tickets :
      t -> ((Ticket_id.t * Amount.t) * Ticket_handle.t) list -> t

    val tickets_mapping : t -> ((Ticket_id.t * Amount.t) * Ticket_handle.t) list
  end

  module Origination_payload : sig
    type t [@@deriving yojson]

    val wasm_of_yojson : code:bytes -> storage:bytes -> (t, string) result
  end

  module Compiler : sig
    val compile :
      Origination_payload.t ->
      gas:int ->
      tickets:((Ticket_id.t * Amount.t) * Ticket_handle.t) Seq.t ->
      (Contract.t, string) result
  end

  module Invocation_payload : sig
    type t [@@deriving yojson]

    val wasm_of_yojson : arg:Yojson.Safe.t -> (t, string) result

    val of_bytes : arg:bytes -> (t, string) result
  end

  module Interpreter : sig
    val invoke :
      Contract.t ->
      ctx:
        < State.table_access
        ; State.addressing
        ; State.with_operations
        ; State.finalization
        ; .. > ->
      to_replace:(Int64.t option * Ticket_handle.t) list option ->
      arg:Invocation_payload.t ->
      gas:int ->
      (* TODO: unit should be user operation list *)
      (Contract.t * int list, string) result
  end
end

module Wasm = struct
  module Wasm_tickets : sig
    include Helpers.Map.S with type key := Ticket_id.t * Amount.t

    val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t

    val of_yojson :
      (Yojson.Safe.t -> ('a, string) result) ->
      Yojson.Safe.t ->
      ('a t, string) result
  end = Helpers.Map.Make_with_yojson (struct
    type t = Ticket_id.t * Amount.t [@@deriving yojson]

    let compare a b = Poly.(compare a b)
  end)

  module Contract = struct
    type t = {
      code : Wasm_vm.Module.t;
      storage : bytes;
      tickets : Ticket_handle.t Wasm_tickets.t;
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

    let make ~code ~storage =
      let%ok () =
        Wasm_vm.Module.of_string ~code:(Bytes.to_string code)
        |> Result.map_error ~f:(Fun.const "invalid module")
        |> Result.map ~f:(Fun.const ()) in
      Ok { code; storage }
  end

  module Compiler = struct
    let compile ~gas:_ ~tickets (code : Origination_payload.t) =
      let%ok compiled =
        Wasm_vm.Module.of_string ~code:(Bytes.to_string code.code) in
      Ok
        Contract.
          {
            code = compiled;
            storage = code.storage;
            tickets = Wasm_tickets.of_seq tickets;
          }
  end

  module Invocation_payload = struct
    type t = bytes [@@deriving yojson]
  end

  module Interpreter = struct
    let invoke ctx contract arg ~gas =
      let Contract.{ code; storage; tickets } = contract in
      let custom = FFI.custom ~ctx in
      let gas = ref gas in
      let%ok updated, operations =
        Wasm_vm.Runtime.invoke custom ~module_:code ~storage ~gas ~argument:arg
      in
      let contract = Contract.{ code; storage = updated; tickets } in
      Ok (contract, operations)
  end
end

module Contract = struct
  type t = Wasm of Wasm.Contract.t [@@deriving yojson, eq]

  let raw_storage t =
    match t with
    | Wasm c -> c.storage

  let tickets_mapping = function
    | Wasm c -> c.tickets |> Wasm.Wasm_tickets.bindings

  let update_tickets t tickets =
    match t with
    | Wasm c ->
      Wasm
        {
          c with
          tickets = Wasm.Wasm_tickets.of_seq (Stdlib.List.to_seq tickets);
        }
end

module Origination_payload = struct
  type t = Wasm of Wasm.Origination_payload.t [@@deriving yojson]

  let wasm_of_yojson ~code ~storage =
    let%ok payload = Wasm.Origination_payload.make ~code ~storage in
    Ok (Wasm payload)
end

module Compiler = struct
  let compile payload ~gas ~tickets =
    match payload with
    | Origination_payload.Wasm contract ->
      let%ok contract =
        Wasm.Compiler.compile contract ~gas ~tickets
        |> Result.map_error ~f:Wasm_vm.Errors.show in
      Ok (Contract.Wasm contract)
end

module Invocation_payload = struct
  type t = Wasm of Wasm.Invocation_payload.t [@@deriving yojson]

  let wasm_of_yojson ~arg =
    let%ok arg = Wasm.Invocation_payload.of_yojson arg in
    Ok (Wasm arg)

  let of_bytes ~arg = Ok (Wasm arg)
end

module Interpreter = struct
  let invoke code ~ctx ~to_replace ~arg ~gas =
    match (code, arg) with
    | Contract.Wasm contract, Invocation_payload.Wasm arg ->
      let%ok arg =
        try
          match to_replace with
          | None -> Ok arg
          | Some lst ->
            let () =
              List.iter
                ~f:(fun (t, h) ->
                  let dst_pos = Int64.to_int_exn (Option.value_exn t) in
                  let handle = Ticket_handle.to_bytes h in
                  Bytes.blit ~src:handle ~src_pos:0 ~dst:arg ~dst_pos ~len:4)
                lst in
            Ok arg
        with
        | _ -> Error "execution_failure" in
      let%ok contract, operations =
        Wasm.Interpreter.invoke ctx contract arg ~gas
        |> Result.map_error ~f:Wasm_vm.Errors.show in
      Ok (Contract.Wasm contract, operations)
end
