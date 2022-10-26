open Helpers

type ticket_id =
  { ticketer : string
  ; data : bytes
  }
[@@deriving ord, eq, yojson]

module rec V : sig
  type union =
    | Left of t
    | Right of t

  and t =
    | Int of Z.t
    | String of string
    | Bytes of bytes
    | Bool of int
    | Pair of t * t
    | Union of union
    | List of t list
    | Option of t option
    | Unit
    | Map of t Map.t
    | Ticket of
        { ticket_id : ticket_id
        ; amount : Z.t
        }
    | Set of Set.t
  [@@deriving ord, eq, yojson]

  val pp : Format.formatter -> t -> unit
end = struct
  type union =
    | Left of t
    | Right of t

  and t =
    | Int of Z.t
    | String of string
    | Bytes of bytes
    | Bool of int
    | Pair of t * t
    | Union of union
    | List of t list
    | Option of t option
    | Unit
    | Map of t Map.t
    | Ticket of
        { ticket_id : ticket_id
        ; amount : Z.t
        }
    | Set of Set.t
  [@@deriving ord, eq, yojson]

  let rec pp fmt t =
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
    | Pair (fst, snd) -> fprintf fmt "(Pair %a %a)" pp fst pp snd
    | Union (Left value) -> fprintf fmt "(Left %a)" pp value
    | Union (Right value) -> fprintf fmt "(Right %a)" pp value
    | List elements -> print_list pp elements
    | Option None -> pp_print_string fmt "None"
    | Option (Some value) -> fprintf fmt "(Some %a)" pp value
    | Unit -> pp_print_string fmt "Unit"
    | Map m ->
      print_list
        (fun fmt (key, value) -> fprintf fmt "Elt %a %a" pp key pp value)
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
    | Set s -> print_list pp (List.of_seq (Set.to_seq s))
    | Ticket t ->
      fprintf fmt "Pair %s %s %a" t.ticket_id.ticketer
        (Bytes.to_string t.ticket_id.data)
        Z.pp_print t.amount
end

and Map : (Helpers.Map.S_with_yojson with type key = V.t) =
  Helpers.Map.Make_with_yojson (V)

and Set : (Helpers.Set.S_with_yojson with type elt = V.t) =
  Helpers.Set.Make_with_yojson (V)

include V
