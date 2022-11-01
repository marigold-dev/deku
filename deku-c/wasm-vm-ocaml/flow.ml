open Deku_stdlib
open Operations
open Effects

exception Runtime of [ `Decode | `Link | `Runtime | `Doesnt_exist ]

let show_error : [ `Decode | `Link | `Runtime | `Doesnt_exist ] -> string =
  function
  | `Decode -> "Decoding error"
  | `Link -> "Linking error"
  | `Runtime -> "Runtime error"
  | `Doesnt_exist -> "Contract doesnt exist"

let apply raw_operation_hash state table tickets op =
  match op with
  | Originate
      {
        initial_storage : Value.t;
        module_ : string;
        entrypoints : Entrypoints.t;
        constants : (int * Value.t) array;
      } ->
      let decoded =
        try
          let s = Hex.to_string (`Hex module_) in
          let res = Wasm.Decode.decode "" s in
          res.it
        with Wasm.Decode.Code (_, _) -> raise (Runtime `Decode)
      in
      let entry =
        State_entry.make ~entrypoints ~constants
          ~source:(Effects.source_addr ()) ~storage:initial_storage
          ~module_:decoded ~encoded_module:module_ ()
      in
      let address =
        Deku_ledger.Contract_address.of_user_operation_hash raw_operation_hash
      in
      deposit_tickets
        (Deku_ledger.Address.of_contract_address (address, None), tickets);
      (State.add_contract state address entry, [])
  | Call { address; argument } -> (
      let address', entrypoint =
        match Deku_ledger.Address.to_contract_address address with
        | None -> raise (Runtime `Decode)
        | Some x -> x
      in
      let (State_entry.Entry
             {
               storage;
               originated_by = _;
               entrypoints;
               constants;
               module_;
               encoded_module = _;
             } as entry) =
        try State.fetch_contract state address'
        with Not_found -> raise (Runtime `Doesnt_exist)
      in
      let contract_tickets = take_tickets address |> List.to_seq in
      let input_tickets = tickets |> List.to_seq in
      let seq = Seq.append contract_tickets input_tickets in
      let temp = Temporary_table.from_seq seq in
      let argument = Temporary_table.to_runtime_ticket temp table argument in
      let storage = Temporary_table.to_runtime_ticket temp table storage in
      let argument =
        let go path acc =
          let open Value in
          List.fold_right
            (fun x acc ->
              match x with
              | Entrypoints.Left -> Union (Left acc)
              | Entrypoints.Right -> Union (Right acc))
            path acc
        in
        match entrypoint with
        | None -> argument
        | Some x when x = "" -> argument
        | Some x ->
            let path =
              match Entrypoints.get_path entrypoints x with
              | None -> raise (Runtime `Link)
              | Some x -> x
            in
            go path argument
      in
      let inst =
        Wasm.Eval.init
          Wasm.(Source.( @@ ) module_ Source.no_region)
          (Imports.imports ()) ~gas_limit:Int64.max_int
      in
      set_instance inst;
      set_constants constants;
      let main =
        match Externs.(extract_func inst main) with
        | Error _ -> raise (Runtime `Link)
        | Ok x -> x
      in
      let res =
        Wasm.Eval.invoke main
          [ Wasm.Values.(Num (I64 (Imports.alloc (argument, storage)))) ]
      in
      match res with
      | Wasm.Values.[ Num (I64 i) ] -> (
          let result = Imports.read i in
          match result with
          | Pair (List (op_list, _), storage) ->
              let values, to_deposit =
                Temporary_table.from_runtime_ticket [] table storage
              in
              let ops =
                List.map
                  (fun x -> Temporary_table.from_runtime_ticket [] table x)
                  op_list
              in
              let new_entry = State_entry.update entry ~storage:values in
              let new_state = State.add_contract state address' new_entry in
              deposit_tickets
                ( Deku_ledger.Address.of_contract_address (address', None),
                  to_deposit );
              Ticket_table.reset table;
              (new_state, ops)
          | _ -> raise (Runtime `Runtime))
      | _ -> raise (Runtime `Runtime))
  | View _ -> failwith "todo"
