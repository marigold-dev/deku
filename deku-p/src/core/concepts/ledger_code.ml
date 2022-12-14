let () = assert (Sys.word_size = 64)

type ledger_code = int
and t = ledger_code [@@deriving eq, ord, show, yojson]

let zero = 0

let next code =
  let code = code + 1 in
  match code >= 0 with true -> Some code | false -> None

let ( < ) (a : ledger_code) (b : ledger_code) = a < b

let encoding =
  let open Data_encoding in
  conv_with_guard
    (fun n -> Int64.of_int n)
    (fun n ->
      let n = Int64.to_int n in
      match n >= 0 with true -> Ok n | false -> Error "negative ledger_code")
    Data_encoding.int64

let to_int code = code

module Map = Map.Make (struct
  type t = ledger_code [@@deriving ord]
end)
