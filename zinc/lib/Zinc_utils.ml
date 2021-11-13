module Nothing = struct
  include Base.Nothing

  let to_yojson = unreachable_code

  let of_yojson _ = failwith "tried to create a void out of yojson"
end

module Z = struct
  include Z

  let to_yojson x = `String (Z.to_string x)

  let of_yojson = function
    | `String s -> Result.Ok (Z.of_string s)
    | _ -> Result.Error "JSON string expected"

  let pp : Format.formatter -> t -> unit =
   fun fmt v -> Format.fprintf fmt "%s" (Z.to_string v)
end

type label = Label of string [@@deriving show {with_path = false}, eq, yojson]

module LMap = struct
  include Map.Make (struct
    type t = label

    let compare = compare
  end)

  type 'a association_list = (label * 'a) list [@@deriving yojson]

  let of_list (lst : 'a association_list) : 'a t =
    let aux prev (k, v) = add k v prev in
    List.fold_left aux empty lst

  let pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
      =
   fun value ppf m ->
    let lst = bindings m in
    let lst =
      Base.List.dedup_and_sort
        ~compare:(fun (Label a, _) (Label b, _) -> String.compare a b)
        lst
    in
    let new_pp ppf (k, v) =
      Format.fprintf ppf "@[<h>%a -> %a@]" pp_label k value v
    in
    Format.fprintf
      ppf
      "%a"
      (Format.pp_print_list
         ?pp_sep:(Some (fun fmt _ -> Format.pp_print_string fmt ", "))
         new_pp)
      lst

  let of_yojson a m = Result.map of_list (association_list_of_yojson a m)

  let to_yojson a m = bindings m |> association_list_to_yojson a
end

module Blake2B_20 = Digestif.Make_BLAKE2B (struct
  let digest_size = 20
end)
