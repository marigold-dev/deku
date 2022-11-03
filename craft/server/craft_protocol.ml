type block_kind = int [@@deriving show]
(** Block kind 0..24. See item.h *)

type coord = int * int * int [@@deriving ord]
type chunk = { p : int; q : int } [@@deriving show, ord]

type block = { chunk : chunk; x : int; y : int; z : int; w : block_kind }
[@@deriving show]

type sign = { x : int; y : int; z : int; face : int; text : string }
[@@deriving show]

type player_position = {
  x : float;
  y : float;
  z : float;
  rx : float;
  ry : float;
}
[@@deriving show]

type player_state = { position : player_position; nick : string }
[@@deriving show]

module Client_commands = struct
  type t =
    | Version  (** unused for now *)
    | Authentication  (** unused for now *)
    | Chunk of { p : int; q : int; key : int }
    | Position of player_position
    | Block of { x : int; y : int; z : int; w : block_kind }
    | Light of {
        x : int;
        y : int;
        z : int;
        w : block_kind;  (** TODO: is w necessary here? *)
      }
    | Sign of sign
    | Talk of string
  [@@deriving show]

  let deserialize str =
    match String.split_on_char ',' str with
    | "V" :: _ -> Version
    | "A" :: _ -> Authentication
    | "C" :: p :: q :: key :: _ ->
        let p, q, key = (int_of_string p, int_of_string q, int_of_string key) in
        Chunk { p; q; key }
        (* P,15.24,12.75,-5.12,4.75,0.13 *)
    | "P" :: x :: y :: z :: rx :: ry :: _ ->
        let x, y, z, rx, ry =
          ( float_of_string x,
            float_of_string y,
            float_of_string z,
            float_of_string rx,
            float_of_string ry )
        in
        Position { x; y; z; rx; ry }
    | "B" :: x :: y :: z :: w :: _ ->
        let x, y, z, w =
          (int_of_string x, int_of_string y, int_of_string z, int_of_string w)
        in
        Block { x; y; z; w }
    | _ ->
        raise
        @@ Invalid_argument (Format.sprintf "Unable to parse command %s" str)
end

module Server_commands = struct
  type player = { player_id : int; player_state : player_state }
  [@@deriving show]

  type t =
    | You of { player_id : int; player_position : player_position }
    | Block of block
    | Light of block
    | Position of { player_id : int; player_position : player_position }
    | Disconnect of int
    | Key of { chunk : chunk; cache_key : int }
    | Redraw of chunk
    | Time of { elapsed : float; day_length : int }
    | Talk of string
    | Nick of { player_id : int; name : string }
    | Sign of sign
  [@@deriving show]

  let serialize (x : t) =
    match x with
    | You { player_id; player_position = { x; y; z; rx; ry } } ->
        Format.sprintf "U,%d,%f,%f,%f,%f,%f" player_id x y z rx ry
    | Block { chunk = { p; q }; x; y; z; w } ->
        Format.sprintf "B,%d,%d,%d,%d,%d,%d" p q x y z w
    | Position { player_id; player_position = { x; y; z; rx; ry } } ->
        Format.sprintf "P,%d,%f,%f,%f,%f,%f" player_id x y z rx ry
    | Talk message -> Format.sprintf "T,%s" message
    | Redraw { p; q } -> Format.sprintf "R,%d,%d" p q
    | Light { chunk = { p; q }; x; y; z; w } ->
        Format.sprintf "L,%d,%d,%d,%d,%d,%d" p q x y z w
    | Disconnect player_id -> Format.sprintf "D,%d" player_id
    | Key { chunk = { p; q }; cache_key } ->
        Format.sprintf "K,%d,%d,%d" p q cache_key
    | Time { elapsed; day_length } ->
        Format.sprintf "E,%f,%d" elapsed day_length
    | Nick { player_id; name } -> Format.sprintf "N,%d,%s" player_id name
    | Sign { x; y; z; face; text } ->
        Format.sprintf "S,%d,%d,%d,%d,%s" x y z face text
end
