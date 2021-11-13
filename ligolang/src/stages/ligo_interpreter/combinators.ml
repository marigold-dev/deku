open Types

let v_pair : value * value -> value =
  fun (a,b) -> V_Record (LMap.of_list [(Label "0", a) ; (Label "1",b)])

let v_bool : bool -> value =
  fun b -> V_Ct (C_bool b)

let v_unit : unit -> value =
  fun () -> V_Ct (C_unit)

let v_string : string -> value =
  fun s -> V_Ct (C_string s)

let v_some : value -> value =
  fun v -> V_Construct ("Some", v)

let v_none : unit -> value =
  fun () -> V_Construct ("None", v_unit ())

let v_ctor : string -> value -> value =
  fun ctor value -> V_Construct (ctor, value)

let v_address : Tezos_protocol_011_PtHangzH.Protocol.Alpha_context.Contract.t -> value =
  fun a -> V_Ct (C_address a)

let extract_pair : value -> (value * value) option =
  fun p ->
    ( match p with
      | V_Record lmap ->
        let fst = LMap.find (Label "0") lmap in
        let snd = LMap.find (Label "1") lmap in
        Some (fst,snd)
      | _ -> None
    )

let extract_fold_while_result : value -> (bool * value) option =
  fun p ->
    match extract_pair p with
    | Some (V_Ct (C_bool a),b) -> Some (a,b)
    | _ -> None

let is_true : value -> bool =
  fun b -> match b with
    | V_Ct (C_bool true) -> true
    | _ -> false

let is_bool : value -> bool =
  fun b -> match b with
    | V_Ct (C_bool _) -> true
    | _ -> false

let counter_of_address : string -> int = fun addr ->
  try (int_of_string addr) with | Failure _ -> -1

let get_address : value -> Tezos_protocol_011_PtHangzH.Protocol.Alpha_context.Contract.t option = function
  | V_Ct ( C_address x ) -> Some x
  | _ -> None

let get_michelson_contract : value -> unit Tezos_utils.Michelson.michelson option = function
  | V_Michelson ( Contract x ) -> Some x
  | _ -> None

let get_michelson_expr : value -> (unit Tezos_utils.Michelson.michelson * unit Tezos_utils.Michelson.michelson * Ast_typed.type_expression) option =
  function
  | V_Michelson ( Ty_code x ) -> Some x
  | _ -> None

let get_nat : value -> Z.t option =
  function
  | V_Ct ( C_nat x) -> Some x
  | _ -> None

let get_mutez : value -> Z.t option =
  function
  | V_Ct ( C_mutez x) -> Some x
  | _ -> None

let get_nat_option : value -> z option option =
  function
  | V_Construct ("Some", V_Ct (C_nat x)) -> Some (Some x)
  | V_Construct ("None", V_Ct C_unit) -> Some (None)
  | _ -> None

let get_int : value -> Z.t option =
  function
  | V_Ct ( C_int x) -> Some x
  | _ -> None

let get_string : value -> string option =
  function
  | V_Ct (C_string x) -> Some x
  | _ -> None

let get_string_option : value -> string option option =
  function
  | V_Construct ("Some", V_Ct (C_string x)) -> Some (Some x)
  | V_Construct ("None", V_Ct C_unit) -> Some (None)
  | _ -> None

let get_option : value -> value option option =
  fun value ->
    match value with
    | V_Construct ("Some", v) -> Some (Some v)
    | V_Construct ("None", _) -> Some None
    | _ -> None

let get_map : value -> (value * value) list option =
  fun value ->
    match value with
    | V_Map v -> Some v
    | _ -> None

let get_set : value -> value list option =
  fun value ->
    match value with
    | V_Set v -> Some v
    | _ -> None

let get_list : value -> value list option =
  fun value ->
    match value with
    | V_List v -> Some v
    | _ -> None

let get_pair : value -> (value * value) option =
  fun value ->
    match value with
    | V_Record lm -> (
      let x = LMap.to_kv_list lm in
      match x with
      | [ (Label "0", x ) ; (Label "1", y) ] -> Some (x,y)
      | _ -> None
    )
    | _ -> None

let get_func : value -> func_val option =
  fun value ->
    match value with
    | V_Func_val v -> Some v
    | _ -> None

let compare_constant_val (c : constant_val) (c' : constant_val) : int =
  match c, c' with
  | C_unit, C_unit -> Unit.compare () ()
  | C_unit, (C_bool _ | C_int _ | C_nat _ | C_timestamp _ | C_string _ | C_bytes _ | C_mutez _ | C_address _ | C_contract _ | C_key_hash _) -> -1
  | C_bool _, C_unit -> -1
  | C_bool b, C_bool b' -> Bool.compare b b'
  | C_bool _, (C_int _ | C_nat _ | C_timestamp _ | C_string _ | C_bytes _ | C_mutez _ | C_address _ | C_contract _ | C_key_hash _) -> 1
  | C_int _, (C_unit | C_bool _) -> - 1
  | C_int i, C_int i' -> Z.compare i i'
  | C_int _, (C_nat _ | C_timestamp _ | C_string _ | C_bytes _ | C_mutez _ | C_address _ | C_contract _ | C_key_hash _) -> 1
  | C_nat _, (C_unit | C_bool _ | C_int _) -> - 1
  | C_nat n, C_nat n' -> Z.compare n n'
  | C_nat _, (C_timestamp _ | C_string _ | C_bytes _ | C_mutez _ | C_address _ | C_contract _ | C_key_hash _) -> 1
  | C_timestamp _, (C_unit | C_bool _ | C_int _ | C_nat _) -> - 1
  | C_timestamp t, C_timestamp t' -> Z.compare t t'
  | C_timestamp _, (C_string _ | C_bytes _ | C_mutez _ | C_address _ | C_contract _ | C_key_hash _) -> 1
  | C_string _, (C_unit | C_bool _ | C_int _ | C_nat _| C_timestamp _) -> - 1
  | C_string s, C_string s' -> String.compare s s'
  | C_string _, (C_bytes _ | C_mutez _ | C_address _ | C_contract _ | C_key_hash _) -> 1
  | C_bytes _, (C_unit | C_bool _ | C_int _ | C_nat _| C_timestamp _ | C_string _) -> - 1
  | C_bytes b, C_bytes b' -> Bytes.compare b b'
  | C_bytes _ , (C_mutez _ | C_address _ | C_contract _ | C_key_hash _) -> 1
  | C_mutez _, (C_unit | C_bool _ | C_int _ | C_nat _| C_timestamp _ | C_string _ | C_bytes _) -> - 1
  | C_mutez m, C_mutez m' -> Z.compare m m'
  | C_mutez _ , (C_address _ | C_contract _ | C_key_hash _) -> 1
  | C_address _, (C_unit | C_bool _ | C_int _ | C_nat _| C_timestamp _ | C_string _ | C_bytes _ | C_mutez _) -> - 1
  | C_address a, C_address a' ->
     Tezos_protocol_011_PtHangzH.Protocol.Alpha_context.Contract.compare a a'
  | C_address _ , (C_contract _ | C_key_hash _) -> 1
  | C_contract _, (C_unit | C_bool _ | C_int _ | C_nat _| C_timestamp _ | C_string _ | C_bytes _ | C_mutez _ | C_address _) -> - 1
  | C_contract {address=a;entrypoint=e}, C_contract {address=a';entrypoint=e'} -> (
     match Tezos_protocol_011_PtHangzH.Protocol.Alpha_context.Contract.compare a a' with
       0 -> Option.compare String.compare e e'
     | c -> c
  )
  | C_contract _ , C_key_hash _ -> 1
  | C_key_hash _, (C_unit | C_bool _ | C_int _ | C_nat _| C_timestamp _ | C_string _ | C_bytes _ | C_mutez _ | C_address _ | C_contract _) -> - 1
  | C_key_hash kh, C_key_hash kh' ->
     Tezos_crypto.Signature.Public_key_hash.compare kh kh'

let rec compare_value (v : value) (v' : value) : int =
  match v, v' with
  | V_Ct c, V_Ct c' -> compare_constant_val c c'
  | V_Ct _, (V_List _ | V_Record _ | V_Map _ | V_Set _ | V_Construct _ | V_Michelson _ | V_Ligo _ | V_Mutation _ | V_Failure _ | V_Func_val _) -> 1
  | V_List _, V_Ct _ -> - 1
  | V_List l, V_List l' -> List.compare compare_value l l'
  | V_List _, (V_Record _ | V_Map _ | V_Set _ | V_Construct _ | V_Michelson _ | V_Ligo _ | V_Mutation _ | V_Failure _ | V_Func_val _) -> 1
  | V_Record _, (V_Ct _ | V_List _) -> -1
  | V_Record r, V_Record r' ->
     let compare (Label l, v) (Label l', v') = match String.compare l l' with
         0 -> compare_value v v'
       | c -> c in
     let r = LMap.to_kv_list r |> List.sort ~compare in
     let r' = LMap.to_kv_list r' |> List.sort ~compare in
     List.compare compare r r'
  | V_Record _, (V_Map _ | V_Set _ | V_Construct _ | V_Michelson _ | V_Ligo _ | V_Mutation _ | V_Failure _ | V_Func_val _) -> 1
  | V_Map _, (V_Ct _ | V_List _ | V_Record _) -> -1
  | V_Map m, V_Map m' ->
     let compare (k1, v1) (k2, v2) = match compare_value k1 k2 with
         0 -> compare_value v1 v2
       | c -> c in
     let m = List.sort ~compare m in
     let m' = List.sort ~compare m' in
    List.compare compare m m'
  | V_Map _, (V_Set _ | V_Construct _ | V_Michelson _ | V_Ligo _ | V_Mutation _ | V_Failure _ | V_Func_val _) -> 1
  | V_Set _, (V_Ct _ | V_List _ | V_Record _ | V_Map _) -> -1
  | V_Set s, V_Set s' ->
     List.compare compare_value s s'
  | V_Set _, (V_Construct _ | V_Michelson _ | V_Ligo _ | V_Mutation _ | V_Failure _ | V_Func_val _) -> 1
  | V_Construct _, (V_Ct _ | V_List _ | V_Record _ | V_Map _ | V_Set _) -> -1
  | V_Construct (c, l), V_Construct (c', l') -> (
     match String.compare c c' with
       0 -> compare_value l l'
     | c -> c
  )
  | V_Construct _, (V_Michelson _ | V_Ligo _ | V_Mutation _ | V_Failure _ | V_Func_val _) -> 1
  | V_Michelson _, (V_Ct _ | V_List _ | V_Record _ | V_Map _ | V_Set _ | V_Construct _) -> -1
  | V_Michelson m, V_Michelson m' -> (
    match m, m' with
      Contract _, Ty_code _ -> -1
    | Contract c, Contract c' -> compare c c'
    | Ty_code _, Contract _ -> 1
    | Ty_code t, Ty_code t' -> compare t t'
  )
  | V_Michelson _, (V_Ligo _ | V_Mutation _ | V_Failure _ | V_Func_val _) -> 1
  | V_Ligo _, (V_Ct _ | V_List _ | V_Record _ | V_Map _ | V_Set _ | V_Construct _ | V_Michelson _) -> -1
  | V_Ligo (l,v), V_Ligo (l',v') -> (
    match String.compare l l' with
      0 -> String.compare v v'
    | c -> c
  )
  | V_Ligo _, (V_Mutation _ | V_Failure _ | V_Func_val _) -> 1
  | V_Mutation _, (V_Ct _ | V_List _ | V_Record _ | V_Map _ | V_Set _ | V_Construct _ | V_Michelson _ | V_Ligo _) -> -1
  | V_Mutation (l, e), V_Mutation (l', e') -> (
    match Location.compare l l' with
      0 -> compare e e'
    | c -> c
  )
  | V_Mutation _, (V_Failure _ | V_Func_val _) -> 1
  | V_Failure _, (V_Ct _ | V_List _ | V_Record _ | V_Map _ | V_Set _ | V_Construct _ | V_Michelson _ | V_Ligo _ | V_Mutation _) -> -1
  | V_Failure e, V_Failure e' -> compare e e'
  | V_Failure _, V_Func_val _ -> 1
  | V_Func_val _, (V_Ct _ | V_List _ | V_Record _ | V_Map _ | V_Set _ | V_Construct _ | V_Michelson _ | V_Ligo _ | V_Mutation _ | V_Failure _) -> -1
  | V_Func_val f, V_Func_val f' -> compare f f'

let equal_constant_val (c : constant_val) (c' : constant_val) : bool = Int.equal (compare_constant_val c c') 0
let equal_value (v : value) (v' : value) : bool = Int.equal (compare_value v v') 0
