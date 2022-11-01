open Deku_ledger

type t =
  | Entry : {
      encoded_module : string;
      storage : Value.t; (* TODO: We need common format, capnp should do it *)
      originated_by : Address.t;
      entrypoints : Entrypoints.t;
      constants : (int * Value.t) array;
      module_ : Wasm.Ast.module_';
          [@compare fun _ _ -> 0] [@equal fun _ _ -> true] [@opaque]
    }
      -> t
[@@deriving show, ord]

let make ~entrypoints ~encoded_module ~storage ~constants ~source ~module_ () =
  (* TODO: validate tickets *)
  Entry
    {
      encoded_module;
      storage;
      originated_by = source;
      entrypoints;
      module_;
      constants;
    }

let update (Entry t) ~storage = Entry { t with storage }

let encoding =
  let open Data_encoding in
  conv
    (fun (Entry
           {
             encoded_module;
             storage;
             originated_by;
             entrypoints;
             module_ = _;
             constants;
           }) ->
      (encoded_module, storage, originated_by, entrypoints, constants))
    (fun (encoded_module, storage, originated_by, entrypoints, constants) ->
      Entry
        {
          encoded_module;
          storage;
          originated_by;
          entrypoints;
          constants;
          module_ =
            (fun x -> Wasm.Source.(x.it))
              (let x = Hex.to_string (`Hex encoded_module) in
               Wasm.Decode.decode "" x);
        })
    (obj5
       (req "encoded_module" string)
       (req "state" (dynamic_size Value.encoding))
       (req "originated_by" (dynamic_size Address.encoding))
       (req "entrypoints" Entrypoints.encoding)
       (req "constants" (array (tup2 int16 Value.encoding))))

let yojson_of_t t =
  `String
    (Data_encoding.Json.construct encoding t |> Data_encoding.Json.to_string)
