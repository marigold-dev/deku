open Ocaml_wasm_vm

type params = (string * string list) list

module type HANDLERS = sig
  type path
  type body [@@deriving of_yojson]
  type response [@@deriving yojson_of]

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
  type response [@@deriving yojson_of]

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
[@@deriving yojson]

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
  [@@deriving of_yojson]

  type response =
    | Michelson_result of { code : string; storage : string }
    | Wasm_result of Operation_payload.t

  let yojson_of_response = function
    | Michelson_result { code; storage } ->
        (`Assoc [ ("code", `String code); ("storage", `String storage) ]
          : Yojson.Safe.t)
    | Wasm_result operation_payload ->
        Operation_payload.yojson_of_t operation_payload

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
    address : string; [@default ""]
  }
  [@@deriving of_yojson]

  type response =
    | Michelson_expression of string
    | Wasm_payload of Operation_payload.t

  let yojson_of_response = function
    | Michelson_expression str -> (`String str : Yojson.Safe.t)
    | Wasm_payload operation_payload ->
        Operation_payload.yojson_of_t operation_payload

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
  type response = unit [@@deriving yojson]

  let meth = `GET
  let path = Routes.(s "health" /? nil)
  let route = Routes.(path @--> ())
  let handler ~env:_ ~path:_ ~params:_ = Ok ()
end
