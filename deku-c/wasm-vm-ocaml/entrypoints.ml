module Table = Map.Make (String)

type direction = Left | Right [@@deriving eq, show, ord]

let direction_encoding =
  let open Data_encoding in
  string_enum [ ("Left", Left); ("Right", Right) ]

type t = Entrypoints : direction list Table.t -> t
[@@unboxed] [@@deriving eq, ord]

let show (Entrypoints t) =
  [%show: (string * direction list) list] (Table.bindings t)

let get_path (Entrypoints t) entrypoint = Table.find_opt entrypoint t

let pp fmt (Entrypoints t) =
  Format.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@")
       (fun fmt (k, v) ->
         Format.fprintf fmt "%s -> %s" k ([%show: direction list] v)))
    (Table.bindings t)

let encoding =
  let open Data_encoding in
  conv
    (function Entrypoints x -> Table.bindings x)
    (fun x ->
      Entrypoints
        (List.fold_right (fun (k, v) acc -> Table.add k v acc) x Table.empty))
    (assoc (list direction_encoding))

let of_assoc t = Entrypoints (Table.of_seq (List.to_seq t))
