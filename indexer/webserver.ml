(** retrieve the level of the chain **)
let get_level () = Repository.level () |> [%to_yojson: int64]

let run =
  Dream.serve
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             Dream.json (get_level () |> Yojson.Safe.to_string));
       ]