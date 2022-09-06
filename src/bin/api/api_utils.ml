open Deku_stdlib

let input_of_body ~of_yojson request =
  let%await body = Dream.body request in
  let input =
    try body |> Yojson.Safe.from_string |> of_yojson |> Result.ok
    with exn ->
      let msg = Printexc.to_string exn in
      Error (Api_error.invalid_body msg)
  in
  Lwt.return input
