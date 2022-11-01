open Deku_stdlib
open Deku_concepts

type typ = Bytes | String | Other [@@deriving ord, show]

module rec V : sig
  open Deku_ledger

  type union = Left of t | Right of t

  and t =
    | Int of Z.t
    | String of string
    | Bytes of bytes
    | Bool of int
    | Pair of t * t
    | Union of union
    | List of t list * typ
    | Option of t option
    | Unit
    | Map of t Map.t
    | Closure of { opt_arg : t option; call : Int32.t }
    | Ticket of { ticket_id : Ticket_id.t; amount : Amount.t }
    | Ticket_handle of int
    | Set of Set.t
  [@@deriving ord, show]

  val pp_michelson : Format.formatter -> t -> unit
end = struct
  open Deku_ledger

  type union = Left of t | Right of t

  and t =
    | Int of Z.t
        [@printer fun fmt x -> Format.fprintf fmt "%s" @@ Z.to_string x]
    | String of string
    | Bytes of bytes
    | Bool of int
    | Pair of t * t
    | Union of union
    | List of t list * typ
    | Option of t option
    | Unit
    | Map of t Map.t
        [@printer
          fun fmt x ->
            Format.fprintf fmt "%a"
              (fun fmt x ->
                Format.pp_print_list
                  (fun fmt (x, y) -> Format.fprintf fmt "%a %a" pp x pp y)
                  fmt x)
              (Map.bindings x)]
    | Closure of { opt_arg : t option; call : Int32.t }
    | Ticket of { ticket_id : Ticket_id.t; amount : Amount.t }
    | Ticket_handle of int
    | Set of Set.t
        [@printer
          fun fmt x ->
            Format.fprintf fmt "%a"
              (fun fmt x -> Format.pp_print_list pp fmt x)
              (List.of_seq @@ Set.to_seq x)]
  [@@deriving ord, show]

  let rec pp_michelson fmt t =
    let open Format in
    let print_list f lst =
      let rec aux = function
        | [] -> pp_print_string fmt "}"
        | [ elt ] -> fprintf fmt "%a }" f elt
        | elt :: elts ->
            fprintf fmt "%a; " f elt;
            aux elts
      in
      pp_print_string fmt "}";
      aux lst
    in
    match t with
    | Int z -> Z.pp_print fmt z
    | String s -> fprintf fmt "\"%s\"" s
    | Bool 0 -> pp_print_string fmt "False"
    | Bool _ -> pp_print_string fmt "True"
    | Closure _ -> fprintf fmt "%s" "<opaque>"
    | Pair (fst, snd) ->
        fprintf fmt "(Pair %a %a)" pp_michelson fst pp_michelson snd
    | Union (Left value) -> fprintf fmt "(Left %a)" pp_michelson value
    | Union (Right value) -> fprintf fmt "(Right %a)" pp_michelson value
    | List (elements, _) -> print_list pp_michelson elements
    | Option None -> pp_print_string fmt "None"
    | Option (Some value) -> fprintf fmt "(Some %a)" pp_michelson value
    | Unit -> pp_print_string fmt "Unit"
    | Map m ->
        print_list
          (fun fmt (key, value) ->
            fprintf fmt "Elt %a %a" pp_michelson key pp_michelson value)
          (List.of_seq (Map.to_seq m))
    | Bytes b ->
        let map = "01234567890abcdef" in
        pp_print_string fmt "0x";
        Bytes.iter
          (fun c ->
            let c = Char.code c in
            pp_print_char fmt map.[c lsr 4];
            pp_print_char fmt map.[c land 0xf])
          b
    | Set s -> print_list pp_michelson (List.of_seq (Set.to_seq s))
    | Ticket { ticket_id = Ticket_id { ticketer; data }; amount } ->
        fprintf fmt "Pair %a %a %a" Ticket_id.pp_ticketer ticketer
          Format.pp_print_bytes data Amount.pp amount
    | Ticket_handle _ -> failwith "life cycle error"
end

and Map : (Stdlib.Map.S with type key = V.t) = Stdlib.Map.Make (V)

and Set : (Stdlib.Set.S with type elt = V.t) = struct
  include Stdlib.Set.Make (V)
end

include V

let encoding =
  let open Data_encoding in
  let open Deku_ledger in
  let opt a =
    union ~tag_size:`Uint8
      [
        case ~title:"Some" (Tag 13)
          (tup2 (constant "Some") (dynamic_size a))
          (function Some x -> Some ((), x) | _ -> None)
          (fun (_, x) -> Some x);
        case ~title:"None" (Tag 14)
          (tup2 (constant "None") unit)
          (function None -> Some ((), ()) | _ -> None)
          (fun (_, _) -> None);
      ]
  in
  let union_encoder a =
    union ~tag_size:`Uint8
      [
        case ~title:"Left" (Tag 15)
          (tup2 (constant "Left") a)
          (function Left x -> Some ((), x) | _ -> None)
          (fun (_, x) -> Left x);
        case ~title:"Right" (Tag 16)
          (tup2 (constant "Right") a)
          (function Right x -> Some ((), x) | _ -> None)
          (fun (_, x) -> Right x);
      ]
  in
  let mu_value =
    mu "value" ~title:"Value" (fun e ->
        union ~tag_size:`Uint8
          [
            case ~title:"Int" (Tag 0)
              (tup2 (constant "Int") z)
              (function Int x -> Some ((), x) | _ -> None)
              (fun (_, x) -> Int x);
            case ~title:"String" (Tag 1)
              (tup2 (constant "String") string)
              (function String x -> Some ((), x) | _ -> None)
              (fun (_, x) -> String x);
            case ~title:"Bytes" (Tag 2)
              (tup2 (constant "Bytes") bytes)
              (function Bytes x -> Some ((), x) | _ -> None)
              (fun (_, x) -> Bytes x);
            case ~title:"Bool" (Tag 3)
              (tup2 (constant "Bool") uint8)
              (function Bool x -> Some ((), x) | _ -> None)
              (fun (_, x) -> Bool x);
            case ~title:"Pair" (Tag 4)
              (tup2 (constant "Pair") (tup2 (dynamic_size e) e))
              (function Pair (x, y) -> Some ((), (x, y)) | _ -> None)
              (fun (_, (x, y)) -> Pair (x, y));
            case ~title:"List" (Tag 5)
              (tup2 (constant "List") (list (dynamic_size e)))
              (function List (x, _) -> Some ((), x) | _ -> None)
              (fun (_, x) ->
                let typ : typ =
                  match x with
                  | Bytes _ :: _ -> Bytes
                  | String _ :: _ -> String
                  | _ -> Other
                in
                List (x, typ));
            case ~title:"Option" (Tag 6)
              (tup2 (constant "Option") (opt e))
              (function Option x -> Some ((), x) | _ -> None)
              (fun (_, x) -> Option x);
            case ~title:"Set" (Tag 7)
              (tup2 (constant "Set") (list (dynamic_size e)))
              (function
                | Set x -> Some ((), List.of_seq @@ Set.to_seq x) | _ -> None)
              (fun (_, x) -> Set (Set.of_list x));
            case ~title:"Map" (Tag 8)
              (tup2 (constant "Map")
                 (list (tup2 (dynamic_size e) (dynamic_size e))))
              (function Map x -> Some ((), Map.bindings x) | _ -> None)
              (fun (_, x) -> Map (Map.of_seq @@ List.to_seq x));
            case ~title:"Union" (Tag 9)
              (tup2 (constant "Union") (dynamic_size (union_encoder e)))
              (function Union x -> Some ((), x) | _ -> None)
              (fun (_, x) -> Union x);
            case ~title:"Unit" (Tag 10) (tup1 string)
              (function Unit -> Some "Unit" | _ -> None)
              (fun _ -> Unit);
            case ~title:"Ticket" (Tag 11)
              (tup2 (constant "Ticket")
                 (tup2 Ticket_id.encoding Amount.encoding))
              (function
                | Ticket { ticket_id; amount } -> Some ((), (ticket_id, amount))
                | _ -> None)
              (fun (_, (x, y)) -> Ticket { ticket_id = x; amount = y });
            case ~title:"Ticket_handle" (Tag 12)
              (tup2 (constant "Ticket_handle") int64)
              (function
                | Ticket_handle x -> Some ((), Int64.of_int x) | _ -> None)
              (fun (_, x) -> Ticket_handle (Int64.to_int x));
          ])
  in
  mu_value
