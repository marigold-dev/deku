type t =
  | View of {
      address : Deku_ledger.Contract_address.t;
      view_name : string;
      argument : Value.t;
    }
  | Call of { address : Deku_ledger.Address.t; argument : Value.t }
  | Originate of {
      initial_storage : Value.t;
      module_ : string;
      entrypoints : Entrypoints.t;
      constants : (int * Value.t) array;
    }
[@@deriving show]

exception Not_an_operation

let encoding =
  let view_tag = 0
  and view_encoding =
    let open Data_encoding in
    obj3
      (req "address" (dynamic_size Deku_ledger.Contract_address.encoding))
      (req "argument" (dynamic_size Value.encoding))
      (req "view_name" string)
  in
  let call_tag = 1
  and call_encoding =
    let open Data_encoding in
    obj2
      (req "address" (dynamic_size Deku_ledger.Address.encoding))
      (req "argument" Value.encoding)
  in
  let originate_tag = 2
  and originate_encoding =
    let open Data_encoding in
    obj4
      (req "initial_storage" (dynamic_size Value.encoding))
      (req "module" string)
      (req "constants" (dynamic_size (array (tup2 int16 Value.encoding))))
      (req "entrypoints" Entrypoints.encoding)
  in

  let matcher_fun (x : t) =
    let open Data_encoding in
    match x with
    | View { address; view_name; argument } ->
        matched ~tag_size:`Uint8 view_tag view_encoding
          (address, argument, view_name)
    | Call { address; argument } ->
        matched ~tag_size:`Uint8 call_tag call_encoding (address, argument)
    | Originate { initial_storage; module_; entrypoints; constants } ->
        matched ~tag_size:`Uint8 originate_tag originate_encoding
          (initial_storage, module_, constants, entrypoints)
  in
  let open Data_encoding in
  matching ~tag_size:`Uint8 matcher_fun
    [
      case ~title:"View" ~description:"call contract view" (Tag view_tag)
        view_encoding
        (function
          | View { address; argument; view_name } ->
              Some (address, argument, view_name)
          | _ -> None)
        (fun (address, argument, view_name) ->
          View { address; argument; view_name });
      case ~title:"Call" ~description:"call contract" (Tag call_tag)
        call_encoding
        (function
          | Call { address; argument } -> Some (address, argument) | _ -> None)
        (fun (address, argument) -> Call { address; argument });
      case ~title:"Originate" ~description:"originate contract"
        (Tag originate_tag) originate_encoding
        (function
          | Originate { initial_storage; module_; entrypoints; constants } ->
              Some (initial_storage, module_, constants, entrypoints)
          | _ -> None)
        (fun (initial_storage, module_, constants, entrypoints) ->
          Originate { initial_storage; module_; constants; entrypoints });
    ]

let yojson_of_t t =
  `String
    (Data_encoding.Json.construct encoding t |> Data_encoding.Json.to_string)

let t_of_yojson t =
  match t with
  | `String string -> (
      try
        Data_encoding.Json.from_string string
        |> Result.map (fun x -> Data_encoding.Json.destruct encoding x)
        |> Result.get_ok
      with Invalid_argument _ -> raise Not_an_operation)
  | _ -> raise Not_an_operation
