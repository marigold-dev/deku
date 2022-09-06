open Deku_stdlib

let input_of_body ~of_yojson request =
  let%await body = Dream.body request in
  let body =
    try body |> Yojson.Safe.from_string |> Result.ok
    with _ -> Error "invalid body" (*TODO: better error*)
  in
  match body with
  | Error error -> Lwt.return_error error
  | Ok body ->
      let input = try of_yojson body |> Option.some with _ -> None in
      input
      |> Option.to_result ~none:"invalid input"
      |> Lwt.return (* TODO: better error *)