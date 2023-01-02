open Core_bench
open Deku_concepts
open Deku_protocol
open Deku_crypto

module Get_level = struct
  type path = unit
  type response = { level : Level.t }

  let response_encoding =
    let open Data_encoding in
    conv
      (fun { level } -> level)
      (fun level -> { level })
      (obj1 (req "level" Level.encoding))
end

module Encode_operation = struct
  type body = Operation.Initial.hash_repr

  let body_encoding = Operation.Initial.hash_encoding
end

module Operations = struct
  type t = {
    key : Key.t;
    signature : Signature.t;
    initial : Operation.Initial.t;
  }

  let obj_encoding =
    let open Data_encoding in
    conv
      (fun { key; signature; initial } -> (key, signature, initial))
      (fun (key, signature, initial) -> { key; signature; initial })
      (obj3 (req "key" Key.encoding)
         (req "signature" Signature.encoding)
         (req "initial" Operation.Initial.encoding))

  let tup_encoding =
    let open Data_encoding in
    conv
      (fun { key; signature; initial } -> (key, signature, initial))
      (fun (key, signature, initial) -> { key; signature; initial })
      (tup3 Key.encoding Signature.encoding Operation.Initial.encoding)

  let to_signed repr =
    let { key; signature; initial } = repr in
    Operation.Signed.make_with_signature ~key ~signature ~initial
end

let () =
  let json_level_encoding = {| { "level" : 548430 } |} in
  let level : Get_level.response =
    Data_encoding.Json.from_string json_level_encoding
    |> Result.get_ok
    |> Data_encoding.Json.destruct Get_level.response_encoding
  in
  (* let json_level_yojson = {| { "level" : "548430" } |} in *)
  let json_encode_operation_encoding =
    {| {"nonce":"495761182","level":289099,"operation":{"type":"vm_transaction","sender":"tz1KufAGaM2EM49bikm5VQfLNWT9rWsAWEHy","operation":{"operation":{"address":"DK1DpUMB44Ex3WXEUXPjp9DDkjiQ5cyvVwoU","argument":["Union",["Right",["Union",["Left",["Pair",[["Pair",[["Int","136331"],["String","tz1KufAGaM2EM49bikm5VQfLNWT9rWsAWEHy"]]],["Union",["Left",["Union",["Left",["Union",["Right",["Unit"]]]]]]]]]]]]]},"tickets":[]}}}|}
  in
  let encode_operation =
    Data_encoding.Json.from_string json_encode_operation_encoding
    |> Result.get_ok
    |> Data_encoding.Json.destruct Encode_operation.body_encoding
  in
  (* let json_encode_operation_yojson =
       Yojson.Safe.to_string (Encode_operation.yojson_of_body encode_operation)
     in *)
  let json_operation_encoding =
    {| {"key":"edpkuUADDrRjMHLq9ehcWJev6Gd9YLTX38vgXSizzghJ9NS5ceNg8v","signature":"edsigu48RihHyRN7XRm8YjdLzCcF8rMRwF6WWTzGcU3jQYguQhCCv1thksnMSrbEwEhHnRUovfKoSYLaacT6ek3iJLuBgfaARL2","initial":{"nonce":"735632897","level":291078,"operation":{"type":"vm_transaction","sender":"tz1KufAGaM2EM49bikm5VQfLNWT9rWsAWEHy","operation":{"operation":{"address":"DK1DpUMB44Ex3WXEUXPjp9DDkjiQ5cyvVwoU","argument":["Union",["Right",["Union",["Left",["Pair",[["Pair",[["Int","136331"],["String","tz1KufAGaM2EM49bikm5VQfLNWT9rWsAWEHy"]]],["Union",["Left",["Union",["Left",["Union",["Right",["Unit"]]]]]]]]]]]]]},"tickets":[]}}}}|}
  in
  let operation =
    Data_encoding.Json.from_string json_operation_encoding
    |> Result.get_ok
    |> Data_encoding.Json.destruct Operations.obj_encoding
  in
  let binary_operation : string =
    Data_encoding.Binary.to_string_opt Operations.obj_encoding operation
    |> Option.get
  in
  (* let json_operation_yojson =
       Operations.yojson_of_t operation |> Yojson.Safe.to_string
     in *)
  let json_operation_tup =
    Data_encoding.Json.to_string
      (Data_encoding.Json.construct Operations.tup_encoding operation)
  in
  let config : Core_bench_internals.Run_config.t =
    Bench.Run_config.create
      ?verbosity:(Some Core_bench_internals.Verbosity.High)
      ?quota:(Some (Bench.Quota.of_string "10"))
      ?no_compactions:None ?sampling_type:None ?stabilize_gc_between_runs:None
      ?fork_each_benchmark:None ?thin_overhead:None ()
  in
  let measures =
    Bench.measure ?run_config:(Some config)
      [
        Bench.Test.create_group ~name:"level to JSON"
          [
            (* Bench.Test.create ~name:"yojson" (fun () ->
                ignore (Get_level.yojson_of_response level)); *)
            Bench.Test.create ~name:"encoding" (fun () ->
                ignore
                  (Data_encoding.Json.construct Get_level.response_encoding
                     level));
          ];
        Bench.Test.create_group ~name:"Encode_operation to JSON"
          [
            (* Bench.Test.create ~name:"yojson" (fun () ->
                ignore (Encode_operation.yojson_of_body encode_operation)); *)
            Bench.Test.create ~name:"encoding" (fun () ->
                ignore
                  (Data_encoding.Json.construct Encode_operation.body_encoding
                     encode_operation));
          ];
        Bench.Test.create_group ~name:"Operations to JSON"
          [
            (* Bench.Test.create ~name:"yojson" (fun () ->
                ignore (Operations.yojson_of_t operation)); *)
            Bench.Test.create ~name:"encoding" (fun () ->
                ignore
                  (Data_encoding.Json.construct Operations.obj_encoding
                     operation));
          ];
        Bench.Test.create_group ~name:"level from JSON"
          [
            (* Bench.Test.create ~name:"yojson" (fun () ->
                ignore
                  (Get_level.response_of_yojson
                  @@ Yojson.Safe.from_string json_level_yojson)); *)
            Bench.Test.create ~name:"encoding" (fun () ->
                ignore
                  (Data_encoding.Json.from_string json_level_encoding
                  |> Result.get_ok
                  |> Data_encoding.Json.destruct Get_level.response_encoding));
          ];
        Bench.Test.create_group ~name:"Encode_operation from JSON"
          [
            (* Bench.Test.create ~name:"yojson" (fun () ->
                ignore
                  (Encode_operation.body_of_yojson
                  @@ Yojson.Safe.from_string json_encode_operation_yojson)); *)
            Bench.Test.create ~name:"encoding" (fun () ->
                ignore
                  (Data_encoding.Json.from_string json_encode_operation_encoding
                  |> Result.get_ok
                  |> Data_encoding.Json.destruct Encode_operation.body_encoding
                  ));
          ];
        Bench.Test.create_group ~name:"Operations from JSON"
          [
            (* Bench.Test.create ~name:"yojson" (fun () ->
                ignore
                  (Operations.t_of_yojson
                  @@ Yojson.Safe.from_string json_operation_yojson)); *)
            Bench.Test.create ~name:"encoding" (fun () ->
                ignore
                  (Data_encoding.Json.from_string json_operation_encoding
                  |> Result.get_ok
                  |> Data_encoding.Json.destruct Operations.obj_encoding));
          ];
        Bench.Test.create_group ~name:"Operations from JSON + to_SIGNED"
          [
            (* Bench.Test.create ~name:"yojson" (fun () ->
                ignore
                  (Operations.to_signed @@ Operations.t_of_yojson
                  @@ Yojson.Safe.from_string json_operation_yojson)); *)
            Bench.Test.create ~name:"encoding" (fun () ->
                ignore
                  (Data_encoding.Json.from_string json_operation_encoding
                  |> Result.get_ok
                  |> Data_encoding.Json.destruct Operations.obj_encoding
                  |> Operations.to_signed));
          ];
        Bench.Test.create_group ~name:"Operations from JSON + tup"
          [
            Bench.Test.create ~name:"encoding" (fun () ->
                ignore
                  (Data_encoding.Json.from_string json_operation_tup
                  |> Result.get_ok
                  |> Data_encoding.Json.destruct Operations.tup_encoding));
          ];
        Bench.Test.create_group ~name:"Operations from binary"
          [
            Bench.Test.create ~name:"encoding" (fun () ->
                ignore
                  (Data_encoding.Binary.of_string_exn Operations.obj_encoding
                     binary_operation));
          ];
      ]
  in
  let analysis_results : Core_bench.Bench.Analysis_result.t Core.Or_error.t list
      =
    List.map (fun m -> Bench.analyze m) measures
  in
  let result =
    List.map
      (fun r -> match r with Ok r -> r | Error _e -> failwith "error")
      analysis_results
  in
  Bench.display result
