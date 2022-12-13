open Deku_protocol
open Deku_stdlib
open Deku_concepts
open Deku_crypto

type signed_operation = {
  key : Key.t;
  signature : Signature.t;
  initial : Operation.Initial.t;
}

let signed_operation_encoding =
  let open Data_encoding in
  conv
    (fun { key; signature; initial } -> (key, signature, initial))
    (fun (key, signature, initial) -> { key; signature; initial })
    (obj3 (req "key" Key.encoding)
       (req "signature" Signature.encoding)
       (req "initial" Operation.Initial.encoding))

let of_signed signed =
  let (Operation.Signed.Signed_operation { key; signature; initial }) =
    signed
  in
  { key; signature; initial }

let to_signed repr =
  let { key; signature; initial } = repr in
  Operation.Signed.make_with_signature ~key ~signature ~initial

let post_to_api ~sw ~env ~operation =
  let node = "http://127.0.0.1:8080/api/v1/operations" |> Uri.of_string in
  let json =
    Data_encoding.Json.construct signed_operation_encoding operation
    |> Data_encoding.Json.to_string
  in
  print_endline json;
  let body = Piaf.Body.of_string json in
  let post_result = Piaf.Client.Oneshot.post ~body ~sw env node in
  match post_result with
  | Ok response -> (
      match Piaf.Body.to_string response.body with
      | Ok body -> print_endline body
      | Error _ -> print_endline "FAILING TO DECODE BODY")
  | Error _ -> print_endline "FAIL to submit operation"

let make_identity secret =
  secret |> Secret.of_b58 |> Option.get |> Identity.make

type level_response = { level : Level.t }

let level_encoding =
  let open Data_encoding in
  conv
    (fun { level } -> level)
    (fun level -> { level })
    (obj1 (req "level" Level.encoding))

let make_level ~sw ~env =
  let response =
    Piaf.Client.Oneshot.get ~sw env
      (Uri.of_string "http://127.0.0.1:8080/api/v1/chain/level")
  in
  let body =
    match response with
    | Error _ -> failwith "cannot connect to the API"
    | Ok res -> res.body
  in
  let string = Piaf.Body.to_string body in
  let body =
    match string with
    | Error _ -> failwith "cannot parse body"
    | Ok body -> body
  in
  let json = Data_encoding.Json.from_string body in
  match json with
  | Ok json ->
      let level = Data_encoding.Json.destruct level_encoding json in
      level
  | _ -> failwith "cannot decode level"

let get_state ~sw ~env =
  let response =
    Piaf.Client.Oneshot.get ~sw env
      (Uri.of_string
         "https://deku-canonical-vm0.deku-v1.marigold.dev/api/v1/state/unix/DK1RCPwCXaEUHZRYCCR8YDjTxRkuziZvmRrE")
  in
  let body =
    match response with
    | Error err ->
        Piaf.Error.pp_hum Format.err_formatter err;
        failwith "cannot connect to the API"
    | Ok res -> res.body
  in
  let string = Piaf.Body.to_string body in
  let body =
    match string with
    | Error _ -> failwith "cannot parse body"
    | Ok body -> body
  in
  let json = Data_encoding.Json.from_string body in
  match json with
  | Ok json ->
      let state =
        Data_encoding.Json.destruct Ocaml_wasm_vm.State_entry.encoding json
      in
      state
  | _ -> failwith "cannot decode state"

let make_nonce () =
  let rng = Stdlib.Random.State.make_self_init () in
  Stdlib.Random.State.bits64 rng
  |> Int64.abs |> Z.of_int64 |> N.of_z |> Option.get |> Nonce.of_n

let state_to_operation state =
  let (Ocaml_wasm_vm.State_entry.Entry actual_state) = state in
  let initial_storage = actual_state.storage in
  let module_ = actual_state.encoded_module in
  let constants = actual_state.constants in
  let entrypoints = actual_state.entrypoints in
  Ocaml_wasm_vm.Operation.Originate
    { initial_storage; module_; constants; entrypoints }

let main ~env ~sw =
  let identity =
    make_identity "edsk4UWkJqpZrAm26qvJE8uY9ZFGFqQiFuBcDyEPASXeHxuD68WvvF"
  in
  let level = make_level ~sw ~env in
  let nonce = make_nonce () in
  let actual_state = get_state ~sw ~env in
  let operation = state_to_operation actual_state in
  let content3 =
    Data_encoding.Json.to_string
      (Data_encoding.Json.construct Ocaml_wasm_vm.Operation.encoding operation)
  in
  print_endline content3;
  (* let operation = Data_encoding.Json.from_string content3 in *)
  let tickets = [] in
  let operation_payload =
    Ocaml_wasm_vm.Operation_payload.{ operation; tickets }
  in
  print_endline (Ocaml_wasm_vm.Operation_payload.show operation_payload);

  let (Deku_protocol.Operation.Signed.Signed_operation transaction as op) =
    Operation.Signed.vm_transaction ~level:level.level ~nonce
      ~content:operation_payload ~identity
  in
  let (Deku_protocol.Operation.Initial.Initial_operation { hash; _ }) =
    transaction.initial
  in
  Format.printf "hash: %a\n%!" Operation_hash.pp hash;
  let address =
    Deku_ledger.Contract_address.of_user_operation_hash
      (Deku_protocol.Operation_hash.to_blake2b hash)
    |> Deku_ledger.Contract_address.to_b58
  in
  (match operation_payload.operation with
  | Originate _ ->
      print_newline ();
      print_endline ("Address: " ^ address ^ "\n");
      print_newline ()
  | _ -> ());
  let () = post_to_api ~sw ~env ~operation:(of_signed op) in
  exit 0

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> main ~env ~sw
