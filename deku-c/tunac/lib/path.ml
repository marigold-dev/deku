type path =
  | Left
  | Right

let yojson_of_path = function
  | Left -> `String "Left"
  | Right -> `String "Right"

let path_of_yojson = function
  | `String "Left" -> Left
  | `String "Right" -> Right
  | _ -> failwith "bad"

module M = struct
  include Map.Make (String)
end

type t = path list M.t

let t_of_yojson : Yojson.Safe.t -> t = function
  | `Assoc l ->
    List.to_seq l
    |> Seq.map (fun (k, v) -> (k, [%of_yojson: path list] v))
    |> M.of_seq
  | _ -> failwith "FIXME: what to do here?"

let yojson_of_t map =
  let assoc =
    M.bindings map
    |> List.map (fun (k, v) ->
           (* FIXME: doing this for convenience for now, but it seems
              like a bad idea in the long run. We should make the protocol
              agnostic of the serialization format. *)
           let v_json = [%yojson_of: path list] v in
           (k, v_json))
  in
  `Assoc assoc
