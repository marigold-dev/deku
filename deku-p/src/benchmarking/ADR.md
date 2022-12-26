# Architecture Design Record

The goal of the benchmarks is to validate that `Data_encoding` is a better choice than `Yojson` for encoding and decoding.

# Run the benchmarks

```bash
$ dune exec benchmarking

Estimated testing time 2m40s (16 benchmarks x 10s). Change using '-quota'.
level to JSON/yojson: Total time taken 10.050047159194946s (1133 samples, max runs 1650007).
level to JSON/encoding: Total time taken 10.015674114227295s (1164 samples, max runs 2246186).
Encode_operation to JSON/yojson: Total time taken 10.008656024932861s (528 samples, max runs 4061).
Encode_operation to JSON/encoding: Total time taken 10.010571002960205s (648 samples, max runs 13285).
Operations to JSON/yojson: Total time taken 10.093136072158813s (472 samples, max runs 2347).
Operations to JSON/encoding: Total time taken 10.017661809921265s (538 samples, max runs 4481).
level from JSON/yojson: Total time taken 10.00353479385376s (968 samples, max runs 319531).
level from JSON/encoding: Total time taken 10.012985706329346s (899 samples, max runs 160843).
Encode_operation from JSON/yojson: Total time taken 10.066035985946655s (126 samples, max runs 126).
Encode_operation from JSON/encoding: Total time taken 10.131572008132935s (127 samples, max runs 127).
Operations from JSON/yojson: Total time taken 10.051375150680542s (125 samples, max runs 125).
Operations from JSON/encoding: Total time taken 10.132168769836426s (126 samples, max runs 126).
Operations from JSON + to_SIGNED/yojson: Total time taken 10.119817018508911s (124 samples, max runs 124).
Operations from JSON + to_SIGNED/encoding: Total time taken 10.039397239685059s (123 samples, max runs 123).
Operations from JSON + tup/encoding: Total time taken 10.01436185836792s (125 samples, max runs 125).
Operations from binary/encoding: Total time taken 10.070946216583252s (656 samples, max runs 14381).
┌───────────────────────────────────────────┬────────────────┬───────────────┬────────────┬────────────┬─────────────┬──────────┐
│ Name                                      │       Time/Run │       mWd/Run │   mjWd/Run │   Prom/Run │     mGC/Run │ mjGC/Run │
├───────────────────────────────────────────┼────────────────┼───────────────┼────────────┼────────────┼─────────────┼──────────┤
│ level to JSON/yojson                      │        60.72ns │        14.00w │            │            │     0.05e-3 │          │
│ level to JSON/encoding                    │        44.62ns │        49.00w │            │            │     0.19e-3 │          │
│ Encode_operation to JSON/yojson           │    23_642.19ns │    15_284.51w │    150.76w │    150.76w │    58.93e-3 │  0.62e-3 │
│ Encode_operation to JSON/encoding         │     7_413.65ns │     2_869.19w │     44.74w │     44.74w │    11.10e-3 │  0.15e-3 │
│ Operations to JSON/yojson                 │    40_651.36ns │    18_502.42w │    219.23w │    219.23w │    71.28e-3 │  0.70e-3 │
│ Operations to JSON/encoding               │    21_527.34ns │     5_912.69w │     88.59w │     88.59w │    22.80e-3 │  0.25e-3 │
│ level from JSON/yojson                    │       312.34ns │        97.01w │            │            │     0.37e-3 │          │
│ level from JSON/encoding                  │       620.68ns │       514.98w │            │            │     1.96e-3 │          │
│ Encode_operation from JSON/yojson         │ 1_275_840.60ns │ 1_548_138.67w │ 14_092.03w │ 14_092.03w │ 5_937.15e-3 │ 31.47e-3 │
│ Encode_operation from JSON/encoding       │ 1_266_042.20ns │ 1_547_947.82w │ 13_986.71w │ 13_986.71w │ 5_934.20e-3 │ 29.27e-3 │
│ Operations from JSON/yojson               │ 1_297_230.59ns │ 1_552_730.09w │ 14_409.96w │ 14_409.96w │ 5_951.25e-3 │ 28.11e-3 │
│ Operations from JSON/encoding             │ 1_285_833.38ns │ 1_553_656.25w │ 14_203.17w │ 14_203.17w │ 5_952.58e-3 │ 26.02e-3 │
│ Operations from JSON + to_SIGNED/yojson   │ 1_323_388.29ns │ 1_552_965.38w │ 14_464.86w │ 14_464.86w │ 5_948.93e-3 │ 25.02e-3 │
│ Operations from JSON + to_SIGNED/encoding │ 1_339_803.92ns │ 1_553_819.52w │ 14_252.51w │ 14_252.51w │ 5_950.49e-3 │ 23.31e-3 │
│ Operations from JSON + tup/encoding       │ 1_286_578.90ns │ 1_557_523.78w │ 14_198.25w │ 14_198.25w │ 5_963.54e-3 │ 22.08e-3 │
│ Operations from binary/encoding           │     6_904.63ns │     1_155.98w │     14.43w │     14.43w │     4.43e-3 │  0.02e-3 │
└───────────────────────────────────────────┴────────────────┴───────────────┴────────────┴────────────┴─────────────┴──────────┘
```

# What had been tested?

The main goal, is to have some data around `Yojson` VS `Data_encoding`, and also between `Data_encoding.Json` and `Data_encoding.Binary`.

We haven't tested all the record types we defined in Deku, but only the record types, managed by the HTTP API, and which will be called a lot:

- `/level`: a basic JSON like `{"level":"30"}` but it is called by the toolkit each time we want to submit an operation.
- `/encode-operation`: basically, adding the `0x80` prefix for Deku operations, to your submitted operation translated into hexadecimal.
- `/operations`: finally submit the operation to Deku

## /level

### Creating the types

In order to avoid to expose `handlers.ml`, we redefined, inside `benchmarking.ml` the needed types from the HTTP API.

We started by redefining the `Get_level` module, which is using the exposed `Level.t`:

```OCaml
module Get_level = struct
  type path = unit
  type response = { level : Level.t } [@@deriving yojson]

  let response_encoding =
    let open Data_encoding in
    conv
      (fun { level } -> level)
      (fun level -> { level })
      (obj1 (req "level" Level.encoding))
end
```

Since we have added `[@@deriving yojson]` to `response` type, we will need to add several `[@@deriving yojson]` in our code base.

To do a real benchmark, we could have created some `generators` to generate a random `Level.t`, this can be done in a future if needed. For now, we will assume that encoding/decoding any value will have "similar" results.

Hence, we create two different constants, which are representative to what we are going to do inside Deku code:

```OCaml
  let json_level_encoding = {| { "level" : 548430 } |} in
  let level : Get_level.response =
    Data_encoding.Json.from_string json_level_encoding
    |> Result.get_ok
    |> Data_encoding.Json.destruct Get_level.response_encoding
  in
```

`json_level_encoding` is a quoted string representing the `Get_level.response` encoded into a JSON. We "cheat" and decode it to a `Get_level.reponse` using `Data_encoding`, our goal is not to test that the encoding is correct, but how fast it is.

```OCaml
  let json_level_yojson = {| { "level" : "18" } |} in
```

Finally, we defined the exact same `Get_level.response` but from a `yojson` version (TODO: this is only because the current branch doesn't contain a rc-6 commit, which is fixing this breaking change on the HTTP API, once this branch is rebased, remove the previous line).

### Creating the tests

As we want to compare `Yojson` VS `Data_encoding` we defined the two instances, by `[@@deriving yojson]` for `Yojson` and by defining a `response_encoding` for `Data_encoding`.

We then want to test encoding a `Get_level.response` to a JSON, and decoding a JSON to a `Get_level.response`. To do that, using `Core_bench` we simply write the following:

```OCaml
Bench.Test.create_group ~name:"level to JSON"
  [
    Bench.Test.create ~name:"yojson" (fun () ->
        ignore (Get_level.yojson_of_response level));
    Bench.Test.create ~name:"encoding" (fun () ->
        ignore
          (Data_encoding.Json.construct Get_level.response_encoding
             level));
  ];
Bench.Test.create_group ~name:"level from JSON"
  [
    Bench.Test.create ~name:"yojson" (fun () ->
        ignore
          (Get_level.response_of_yojson
          @@ Yojson.Safe.from_string json_level_yojson));
    Bench.Test.create ~name:"encoding" (fun () ->
        ignore
          (Data_encoding.Json.from_string json_level_encoding
          |> Result.get_ok
          |> Data_encoding.Json.destruct Get_level.response_encoding));
  ];
```

We now have two groups test from `core_bench`:

1. `Level to JSON` which contains two tests, creating a JSON from a `Get_level.response`:

   1. ```OCaml
      Get_level.yojson_of_response level
      (* Creating a JSON from a Get_level.response with Yojson *)
      ```

   2. ```OCaml
      Data_encoding.Json.construct Get_level.response_encoding level
       (* Creating a JSON from a Get_level.response with Data_encoding *)
      ```

2. `Level from JSON` which also contains two tests, encoding a `Get_level.response` into a JSON:

   1. ```OCaml
      Get_level.response_of_yojson @@ Yojson.Safe.from_string json_level_yojson
      (* Creating a Get_level.response from a JSON with Yojson *)
      ```

   2. ```OCaml
      Data_encoding.Json.from_string json_level_encoding
      |> Result.get_ok
      |> Data_encoding.Json.destruct Get_level.response_encoding
      (* Creating a Get_level.response from a JSON with Data_encoding *)
      ```

### Results of `to JSON`

```bash
$ dune exec benchmarking

level to JSON/yojson: Total time taken 10.050047159194946s (1133 samples, max runs 1650007).
level to JSON/encoding: Total time taken 10.015674114227295s (1164 samples, max runs 2246186).
┌──────────────────────────┬────────────┬───────────┬──────────┐
│ Name                     │   Time/Run │   mWd/Run │  mGC/Run │
├──────────────────────────┼────────────┼───────────┼──────────┤
│ level to JSON/yojson     │    60.72ns │    14.00w │  0.05e-3 │
│ level to JSON/encoding   │    44.62ns │    49.00w │  0.19e-3 │
└──────────────────────────┴────────────┴───────────┴──────────┘
```

Every figures are interesing. We now know that on a 10s run, we have encoded `1650007` at best from `Get_level.response` to the corresponding JSON using `Yojson`.

But, in the same time, we can encode `2246186` at best using `Data_encoding`.

Which leads to an average `time per run` lower by around 25% in favor of `Data_encoding`: 60.72ns for Yojson VS 44.62ns for Data_encoding.

`mWd/Run` is the amount of words allocated on the minor heap, while `mGC/Run` is the number of `minor collections`.

On our example, we see that `Data_encoding` is 25% faster than `Yojson`, but in the mean, time, it is using 3.5 times more words in the minor heap, and so, needing 3.8 times more minor collections.

### Results of `from JSON`

```bash
$ dune exec benchmarking

level from JSON/yojson: Total time taken 10.00353479385376s (968 samples, max runs 319531).
level from JSON/encoding: Total time taken 10.012985706329346s (899 samples, max runs 160843).
┌──────────────────────────┬────────────┬───────────┬──────────┐
│ Name                     │   Time/Run │   mWd/Run │  mGC/Run │
├──────────────────────────┼────────────┼───────────┼──────────┤
│ level to JSON/yojson     │   312.34ns │    97.01w │  0.37e-3 │
│ level to JSON/encoding   │   620.68ns │   514.98w │  1.96e-3 │
└──────────────────────────┴────────────┴───────────┴──────────┘
```

Unlike encoding, decoding a `Get_level.response` is faster using `Yojson`: 312.34ns VS 620.68ns. Moreover, the other figures are also in favor of `Yojson`.

### Conclusion

**If we are only looking for performance, we should use `Data_encoding` for encoding into JSON, and `Yojson` to decode.**

Let's continue with other types, to verify the results, or not!

## /encode-operation

```OCaml
module Encode_operation = struct
  type body = Operation.Initial.hash_repr [@@deriving yojson]

  let body_encoding = Operation.Initial.hash_encoding
end
```

As we did for `Level` we simply added a `[@@deriving yojson]` on the `Operation.Initial.hash_repr` and everywhere it is needed.

The corresponding `encoding` is already defined in `Operation.Initial`, so we are simply going to use it.

### Creating the tests

Once again, because the encoding of `level` is different between `Yojson` and `Data_encoding` (TODO: remove), we need to define two different sample:

```OCaml
let json_encode_operation_encoding =
  {| {"nonce":"495761182","level":289099,"operation":{"type":"vm_transaction","sender":"tz1KufAGaM2EM49bikm5VQfLNWT9rWsAWEHy","operation":{"operation":{"address":"DK1DpUMB44Ex3WXEUXPjp9DDkjiQ5cyvVwoU","argument":["Union",["Right",["Union",["Left",["Pair",[["Pair",[["Int","136331"],["String","tz1KufAGaM2EM49bikm5VQfLNWT9rWsAWEHy"]]],["Union",["Left",["Union",["Left",["Union",["Right",["Unit"]]]]]]]]]]]]]},"tickets":[]}}}|}
in
let encode_operation =
  Data_encoding.Json.from_string json_encode_operation_encoding
  |> Result.get_ok
  |> Data_encoding.Json.destruct Encode_operation.body_encoding
in
let json_encode_operation_yojson =
  Yojson.Safe.to_string (Encode_operation.yojson_of_body encode_operation)
in
```

Then, we now benchmark the time to encode and decode this `encode-operation` body:

```OCaml
Bench.Test.create_group ~name:"Encode_operation to JSON"
  [
    Bench.Test.create ~name:"yojson" (fun () ->
        ignore (Encode_operation.yojson_of_body encode_operation));
    Bench.Test.create ~name:"encoding" (fun () ->
        ignore
          (Data_encoding.Json.construct Encode_operation.body_encoding
             encode_operation));
  ];
Bench.Test.create_group ~name:"Operations to JSON"
  [
    Bench.Test.create ~name:"yojson" (fun () ->
        ignore (Operations.yojson_of_t operation));
    Bench.Test.create ~name:"encoding" (fun () ->
        ignore
          (Data_encoding.Json.construct Operations.obj_encoding
             operation));
  ];
```

### Results of `to JSON`

```bash
Encode_operation to JSON/yojson: Total time taken 10.008656024932861s (528 samples, max runs 4061).
Encode_operation to JSON/encoding: Total time taken 10.010571002960205s (648 samples, max runs 13285).
┌───────────────────────────────────────────┬────────────────┬───────────────┬────────────┬────────────┬─────────────┬──────────┐
│ Name                                      │       Time/Run │       mWd/Run │   mjWd/Run │   Prom/Run │     mGC/Run │ mjGC/Run │
├───────────────────────────────────────────┼────────────────┼───────────────┼────────────┼────────────┼─────────────┼──────────┤
│ Encode_operation to JSON/yojson           │    23_642.19ns │    15_284.51w │    150.76w │    150.76w │    58.93e-3 │  0.62e-3 │
│ Encode_operation to JSON/encoding         │     7_413.65ns │     2_869.19w │     44.74w │     44.74w │    11.10e-3 │  0.15e-3 │
└───────────────────────────────────────────┴────────────────┴───────────────┴────────────┴────────────┴─────────────┴──────────┘

```

Some new colums are displayed, because these two tests make significant changes on these values, which was not the case for the `Get_level.response`.

As already seen in the previous tests, `Data_encoding` is around 3.2 times faster to encode such a type, but unlike `Get_level.response`, it is also allocating less `minor words` into the heap and ~5.3 times less and needing ~5.3 times less minor collections.

The new colums are:

- `mjWd/Run`: amount of words allocated in the major heap
- `Prom/Run`:
- `mjGC/Run`: amount of major collections

For all these columns, the figures are also in favor of `Data_encoding.

### Results of `from JSON`

```bash
Encode_operation from JSON/yojson: Total time taken 10.066035985946655s (126 samples, max runs 126).
Encode_operation from JSON/encoding: Total time taken 10.131572008132935s (127 samples, max runs 127).
┌───────────────────────────────────────────┬────────────────┬───────────────┬────────────┬────────────┬─────────────┬──────────┐
│ Name                                      │       Time/Run │       mWd/Run │   mjWd/Run │   Prom/Run │     mGC/Run │ mjGC/Run │
├───────────────────────────────────────────┼────────────────┼───────────────┼────────────┼────────────┼─────────────┼──────────┤
│ Encode_operation from JSON/yojson         │ 1_275_840.60ns │ 1_548_138.67w │ 14_092.03w │ 14_092.03w │ 5_937.15e-3 │ 31.47e-3 │
│ Encode_operation from JSON/encoding       │ 1_266_042.20ns │ 1_547_947.82w │ 13_986.71w │ 13_986.71w │ 5_934.20e-3 │ 29.27e-3 │
└───────────────────────────────────────────┴────────────────┴───────────────┴────────────┴────────────┴─────────────┴──────────┘
```

Unlike the previous figures, there is not significant "winner" in this match. `Data_encoding` is globally a bit better, but it is really closed.

### Conclusion

Benchmarking this "complex" record type had lead to display new columns of `core_bench` result because of use of the major heap.

**Looking for performance, `Data_encoding` is better than `Yojson`**

## /operations

This endpoint is used to submit an operation to Deku. Hence, it is called a lot, and since it is carrying a lot of data, it is interesting to benchmark its body.

On this body, we also want to benchmark:

- `obj` and `tup` from `Data_encoding`.
- If the verification of the signature is taking a lot of time or of resources.
- `Data_encoding.Json` VS `Data_encoding.Binary`

### Creating the types

```OCaml
module Operations = struct
  type t = {
    key : Key.t;
    signature : Signature.t;
    initial : Operation.Initial.t;
  }
  [@@deriving yojson]

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
```

Once again, we duplicate the body type from `operations` endpoint in `Handlers.ml`, then adding `[@@deriving yojson]` everywhere it is needed in our Deku code base.

Then we define two different encoding functions:

- `obj_encoding` using `obj` from `Data_encoding`
- `tup_encoding` using `tup` from `Data_encoding`

And finally, we define the exact same `to_signed` function, which verifiy the signature.

### Creating the tests

Then, like for the other tests, we need to define the sample data we are going to encode and decode:

```OCaml
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
let json_operation_yojson =
  Operations.yojson_of_t operation |> Yojson.Safe.to_string
in
let json_operation_tup =
  Data_encoding.Json.to_string
    (Data_encoding.Json.construct Operations.tup_encoding operation)
in
```

And measure everything with `core_bench`:

```OCaml
Bench.Test.create_group ~name:"Operations to JSON"
  [
    Bench.Test.create ~name:"yojson" (fun () ->
        ignore (Operations.yojson_of_t operation));
    Bench.Test.create ~name:"encoding" (fun () ->
        ignore
          (Data_encoding.Json.construct Operations.obj_encoding
             operation));
  ];
Bench.Test.create_group ~name:"Operations from JSON"
  [
    Bench.Test.create ~name:"yojson" (fun () ->
        ignore
          (Operations.t_of_yojson
          @@ Yojson.Safe.from_string json_operation_yojson));
    Bench.Test.create ~name:"encoding" (fun () ->
        ignore
          (Data_encoding.Json.from_string json_operation_encoding
          |> Result.get_ok
          |> Data_encoding.Json.destruct Operations.obj_encoding));
  ];
Bench.Test.create_group ~name:"Operations from JSON + to_SIGNED"
  [
    Bench.Test.create ~name:"yojson" (fun () ->
        ignore
          (Operations.to_signed @@ Operations.t_of_yojson
          @@ Yojson.Safe.from_string json_operation_yojson));
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
```

### Results of `to JSON`

```bash
Operations to JSON/yojson: Total time taken 10.093136072158813s (472 samples, max runs 2347).
Operations to JSON/encoding: Total time taken 10.017661809921265s (538 samples, max runs 4481).
┌───────────────────────────────────────────┬────────────────┬───────────────┬────────────┬────────────┬─────────────┬──────────┐
│ Name                                      │       Time/Run │       mWd/Run │   mjWd/Run │   Prom/Run │     mGC/Run │ mjGC/Run │
├───────────────────────────────────────────┼────────────────┼───────────────┼────────────┼────────────┼─────────────┼──────────┤
│ Operations to JSON/yojson                 │    40_651.36ns │    18_502.42w │    219.23w │    219.23w │    71.28e-3 │  0.70e-3 │
│ Operations to JSON/encoding               │    21_527.34ns │     5_912.69w │     88.59w │     88.59w │    22.80e-3 │  0.25e-3 │
└───────────────────────────────────────────┴────────────────┴───────────────┴────────────┴────────────┴─────────────┴──────────┘
```

Once again, this is a victory for `Data_encoding`, wich is twice ~2 times faster, and also needing less words in minor and major heap, and so, less collections.

### Results of `from JSON`

```bash
Operations from JSON/yojson: Total time taken 10.051375150680542s (125 samples, max runs 125).
Operations from JSON/encoding: Total time taken 10.132168769836426s (126 samples, max runs 126).
Operations from JSON + to_SIGNED/yojson: Total time taken 10.119817018508911s (124 samples, max runs 124).
Operations from JSON + to_SIGNED/encoding: Total time taken 10.039397239685059s (123 samples, max runs 123).
Operations from JSON + tup/encoding: Total time taken 10.01436185836792s (125 samples, max runs 125).
Operations from binary/encoding: Total time taken 10.070946216583252s (656 samples, max runs 14381).
┌───────────────────────────────────────────┬────────────────┬───────────────┬────────────┬────────────┬─────────────┬──────────┐
│ Name                                      │       Time/Run │       mWd/Run │   mjWd/Run │   Prom/Run │     mGC/Run │ mjGC/Run │
├───────────────────────────────────────────┼────────────────┼───────────────┼────────────┼────────────┼─────────────┼──────────┤
│ Operations from JSON/yojson               │ 1_297_230.59ns │ 1_552_730.09w │ 14_409.96w │ 14_409.96w │ 5_951.25e-3 │ 28.11e-3 │
│ Operations from JSON/encoding             │ 1_285_833.38ns │ 1_553_656.25w │ 14_203.17w │ 14_203.17w │ 5_952.58e-3 │ 26.02e-3 │
│ Operations from JSON + to_SIGNED/yojson   │ 1_323_388.29ns │ 1_552_965.38w │ 14_464.86w │ 14_464.86w │ 5_948.93e-3 │ 25.02e-3 │
│ Operations from JSON + to_SIGNED/encoding │ 1_339_803.92ns │ 1_553_819.52w │ 14_252.51w │ 14_252.51w │ 5_950.49e-3 │ 23.31e-3 │
│ Operations from JSON + tup/encoding       │ 1_286_578.90ns │ 1_557_523.78w │ 14_198.25w │ 14_198.25w │ 5_963.54e-3 │ 22.08e-3 │
│ Operations from binary/encoding           │     6_904.63ns │     1_155.98w │     14.43w │     14.43w │     4.43e-3 │  0.02e-3 │
└───────────────────────────────────────────┴────────────────┴───────────────┴────────────┴────────────┴─────────────┴──────────┘
```

#### Yojson VS Data_encoding

There is no significant winner, but `Data_encoding` is faster than `Yojson`

#### With `to_signed` VS without `to_signed`

Unlike previously, when calling `to_signed` function, `Yojson` is faster than `Data_encoding`. In the other hand, there is not a significant win by avoinding to call `to_signed`, 1_285_833.38ns with `Data_encoding` without `to_signed` VS 1_339_803.92ns with `Data_encoding` with `to_signed`

#### `obj` VS `tup`

1_285_833.38ns for `obj_encoding` and 1_286_578.90ns for `tup_encoding`, and other figures are also in favor of `obj`.

#### `Data_encoding.Json` VS `Data_encoding.Binary`

This is a clear win for `Data_encoding.Binary`, where the average `Time/Run` is 6_904.63ns, when it is 1_285_833.38ns for the `Data_encoding.Json`!

### Conclusion

Once again, `Data_encoding.Json` was globally better than `Yojson`.

Inside `Data_encoding`, there is no win to switch from `obj` to `tup.

As expected, decoding with calling the `to_signed` function needs more time and resources than decoding without it. But the win is not significant to remove it.

**The most interesting figure, is about `Data_encoding.Json` VS `Data_encoding.Binary`. This is a clear and neat win for `Data_encoding.Binary`.**

**If we want to improve performances on our HTTP API, we could/should switch the `/operations` endpoint to `Data_encoding.Binary`, however, this would imply to provide a decode/encode function to `Data_encoding.Binary` in our toolkit and without calling our API (because if we simply move it to an other endpoint, we simply move the results).**
