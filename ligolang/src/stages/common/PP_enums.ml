open Format
open Types

let operation ppf (o: bytes) : unit =
  fprintf ppf "%a" Hex.pp (Hex.of_bytes o)

let constant' ppf : constant' -> unit = pp_constant' ppf

let literal ppf (l : literal) =
  match l with
  | Literal_unit -> fprintf ppf "unit"
  | Literal_int z -> fprintf ppf "%a" Z.pp_print z
  | Literal_nat z -> fprintf ppf "+%a" Z.pp_print z
  | Literal_timestamp z -> fprintf ppf "+%a" Z.pp_print z
  | Literal_mutez z -> fprintf ppf "%amutez" Z.pp_print z
  | Literal_string s -> fprintf ppf "%a" Ligo_string.pp s
  | Literal_bytes b -> fprintf ppf "0x%a" Hex.pp (Hex.of_bytes b)
  | Literal_address s -> fprintf ppf "@%S" s
  | Literal_operation o -> fprintf ppf "Operation(%a)" operation o
  | Literal_key s -> fprintf ppf "key %s" s
  | Literal_key_hash s -> fprintf ppf "key_hash %s" s
  | Literal_signature s -> fprintf ppf "Signature %s" s
  | Literal_chain_id s -> fprintf ppf "Chain_id %s" s
