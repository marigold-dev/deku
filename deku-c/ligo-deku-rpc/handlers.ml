open Ocaml_wasm_vm

type params = (string * string list) list

module Operation_payload = struct
  include Operation_payload

  let enc = Data_encoding.Json.convert Operation_payload.encoding
end

module type HANDLERS = sig
  type path
  type body

  val body_encoding : body Json_encoding.encoding

  type response

  val response_encoding : response Json_encoding.encoding
  val meth : [> `GET | `POST ]
  val route : path Routes.route

  val handler :
    env:Eio.Stdenv.t ->
    path:path ->
    params:params ->
    body:body ->
    (response, string) result
end

module type NO_BODY_HANDLERS = sig
  type path
  type response

  val response_encoding : response Data_encoding.t
  val meth : [> `GET | `POST ]
  val route : path Routes.route

  val handler :
    env:Eio.Stdenv.t -> path:path -> params:params -> (response, string) result
end

let version p = Routes.(s "api" / s "v1") p

type lang =
  | Jsligo [@name "jsligo"]
  | Cameligo [@name "mligo"]
  | PascalLigo [@name "ligo"]
  | Michelson [@name "michelson"]
[@@deriving encoding]

let lang_to_string = function
  | Jsligo -> "jsligo"
  | Cameligo -> "mligo"
  | PascalLigo -> "ligo"
  | Michelson -> "michelson"

type compilation_target = Michelson_target | Wasm_target

let rec get_compilation_target = function
  | [] -> Wasm_target
  | ("target", [ "michelson" ]) :: _ -> Michelson_target
  | ("target", [ "wasm" ]) :: _ -> Wasm_target
  | _ :: tl -> get_compilation_target tl

module Compile_contract : HANDLERS = struct
  type path = unit

  type body = { source : string; lang : lang; storage : string }
  [@@deriving encoding]

  let body_encoding = body_enc

  type response =
    | Michelson_result of { code : string; storage : string }
    | Wasm_result of Operation_payload.t
  [@@deriving encoding]

  let response_encoding = response_enc

  let%expect_test "encodings" =
    let show_json kind encoding result =
      let json = Json_encoding.construct encoding result in
      Format.printf "%s:\n%a\n%!" kind Data_encoding.Json.pp json
    in
    show_json "Body JSON" body_encoding
      {
        source = "const add = ([x, y] : [int, int]) : int => { x + y }";
        lang = Jsligo;
        storage = "7";
      };
    show_json "Michelson Response JSON" response_encoding
      (Michelson_result
         { code = "some michelson code"; storage = "some michelson expression" });
    let operation =
      let open Ocaml_wasm_vm in
      let open Deku_tezos in
      let open Deku_ledger in
      let open Deku_concepts in
      let contract_address =
        Contract_hash.of_b58 "KT1LiabSxPyVUmVZCqHneCFLJrqQcLHkmX9d"
        |> Option.get
      in
      let ticketer = Ticket_id.Tezos contract_address in
      let ticket_id = Ticket_id.make ticketer (Bytes.of_string "hello") in
      let address =
        Address.of_b58 "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE" |> Option.get
      in
      let argument = Value.(Union (Left (Union (Right (Int (Z.of_int 5)))))) in
      let operation = Operation.Call { address; argument } in
      (* Currently we don't support passing tickets to this endpoint so
         tickets will always be an emtpy array, but we'll include them
         in this test in case we want to add them in the future *)
      Operation_payload.{ operation; tickets = [ (ticket_id, Amount.zero) ] }
    in
    show_json "WASM Result Response JSON" response_encoding
      (Wasm_result operation);
    [%expect
      {|
      Body JSON:
      { "source": "const add = ([x, y] : [int, int]) : int => { x + y }",
                   "lang": "jsligo", "storage": "7" }
      Michelson Response JSON:
      { "code": "some michelson code",
                                 "storage": "some michelson expression" }
      WASM Result Response JSON:
      { "operation":
                                     { "address":
                                         "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
                                       "argument":
                                         [ "Union",
                                           [ "Left",
                                             [ "Union",
                                               [ "Right", [ "Int", "5" ] ] ] ] ] },
                                   "tickets":
                                     [ [ [ "KT1LiabSxPyVUmVZCqHneCFLJrqQcLHkmX9d",
                                           "68656c6c6f" ], "0" ] ] } |}]

  let meth = `POST
  let path = Routes.(version / s "compile-contract" /? nil)
  let route = Routes.(path @--> ())

  let handler ~env ~path:_ ~params ~body:{ source; lang; storage } =
    let michelson_code, michelson_storage =
      match lang with
      | Michelson -> (source, storage)
      | _ -> (
          let lang = lang_to_string lang in
          let hash = Hash.make source in
          let filename_ligo = Printf.sprintf "%s.%s" hash lang in
          Logs.info (fun m -> m "filename_ligo: %s" filename_ligo);
          let filename_tz = Printf.sprintf "%s.tz" hash in
          Logs.info (fun m -> m "filename_tz: %s" filename_tz);
          Logs.info (fun m -> m "storage: %s" storage);
          let ligo_path = Eio.Path.(Eio.Stdenv.cwd env / filename_ligo) in
          let tz_path = Eio.Path.(Eio.Stdenv.cwd env / filename_tz) in
          let tz_already_exists =
            try Some (Eio.Path.load tz_path) |> Option.is_some with _ -> false
          in
          match tz_already_exists with
          | false ->
              let () =
                try Eio.Path.save ~create:(`Exclusive 0o600) ligo_path source
                with _ -> ()
              in
              let () =
                Ligo_commands.compile_contract ~env ~lang ~filename_ligo
                  ~filename_tz ()
              in
              let code = Eio.Path.load tz_path in
              let storage =
                Ligo_commands.compile_storage ~lang ~filename_ligo
                  ~expression:storage ()
              in
              (code, storage)
          | true ->
              let code = Eio.Path.load tz_path in
              let storage =
                Ligo_commands.compile_storage ~lang ~filename_ligo
                  ~expression:storage ()
                |> String.trim
              in
              (code, storage))
    in
    (* TODO: better error messages in Tuna *)
    let show_tuna_error = function
      | `Parsing_error _ -> "Tuna failed to parse the expression"
      | `Prim_parsing_error error -> Tunac.Michelson_primitives.show_error error
      | `Unexpected_error -> "Tuna encountered an unexpected error"
    in
    match get_compilation_target params with
    | Michelson_target ->
        Ok (Michelson_result { code = michelson_code; storage })
    | Wasm_target -> (
        Logs.info (fun m ->
            m "Compiling michelson storage: %s" michelson_storage);
        match Tunac.Compiler.compile_value michelson_storage with
        | Ok (tickets, init) -> (
            Logs.info (fun m ->
                m "Compiling michelson source:\n%s" michelson_code);
            match Tunac.Compiler.compile michelson_code with
            | Ok (wat, constants, entrypoints) ->
                let out = Tunac.Output.make wat constants |> Result.get_ok in
                let entrypoints = entrypoints |> Option.value ~default:[] in
                Ok
                  (Wasm_result
                     Operation_payload.
                       {
                         tickets;
                         operation =
                           Operation.Originate
                             {
                               module_ = out.module_;
                               entrypoints = Entrypoints.of_assoc entrypoints;
                               constants;
                               initial_storage = init;
                             };
                       })
            | Error err -> Error (show_tuna_error err))
        | Error err -> Error (show_tuna_error err))
end

module Compile_invocation : HANDLERS = struct
  type path = unit

  type body = {
    source : string;
    lang : lang;
    expression : string;
    (* Users may omit the address if they just want michelson  *)
    address : string; [@ddft ""]
  }
  [@@deriving encoding]

  let body_encoding = body_enc

  type response =
    | Michelson_expression of string
    | Wasm_payload of Operation_payload.t
  [@@deriving encoding]

  let response_encoding = response_enc
  let meth = `POST
  let path = Routes.(version / s "compile-invocation" /? nil)
  let route = Routes.(path @--> ())

  let handler ~env ~path:_ ~params ~body:{ source; lang; expression; address } =
    let expression =
      match lang with
      | Michelson -> expression
      | _ ->
          let lang = lang_to_string lang in
          let hash = Hash.make source in
          let filename_ligo = Printf.sprintf "%s.%s" hash lang in
          let ligo_path = Eio.Path.(Eio.Stdenv.cwd env / filename_ligo) in
          let ligo_already_exists =
            try Some (Eio.Path.load ligo_path) |> Option.is_some
            with _ -> false
          in
          (if not ligo_already_exists then
           try Eio.Path.save ~create:(`Exclusive 0o600) ligo_path source
           with _ -> ());
          Ligo_commands.compile_parameter ~lang ~filename_ligo ~expression ()
    in
    match get_compilation_target params with
    | Michelson_target -> Ok (Michelson_expression expression)
    | Wasm_target -> (
        let tickets, init =
          Tunac.Compiler.compile_value expression |> Result.get_ok
        in
        match Deku_ledger.Address.of_b58 address with
        | Some address ->
            Ok
              (Wasm_payload
                 Operation_payload.
                   {
                     tickets;
                     operation = Operation.Call { address; argument = init };
                   })
        | None ->
            Error (Format.sprintf "Unable to parse '%s' as an address" address))
end

module Health : NO_BODY_HANDLERS = struct
  type path = unit
  type response = unit

  let response_encoding = Data_encoding.unit
  let meth = `GET
  let path = Routes.(s "health" /? nil)
  let route = Routes.(path @--> ())
  let handler ~env:_ ~path:_ ~params:_ = Ok ()
end
