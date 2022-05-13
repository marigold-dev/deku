let of_string hexdump =
  let digit c =
    let aux base off = Some (Char.code c - Char.code base + off) in
    match c with
    | '0' .. '9' -> aux '0' 0
    | 'a' .. 'f' -> aux 'a' 10
    | 'A' .. 'F' -> aux 'A' 10
    | _ -> None in
  let _, _, buff, size =
    String.fold_left
      (fun acc c ->
        match acc with
        | None, None, buff, idx -> (digit c, None, buff, idx)
        | (Some _ as d), None, buff, idx -> (d, digit c, buff, idx)
        | Some d1, Some d2, buff, idx ->
          let byte = (d1 lsl 4) lor d2 in
          Bytes.set_int8 buff idx byte;
          (digit c, None, buff, idx + 1)
        | _ -> assert false)
      (None, None, Bytes.create 65536, 0)
      hexdump in
  Bytes.sub buff 0 (size + 1)

let table =
  "0123456789ABCDEF" |> String.to_seq |> Seq.map Char.code |> Array.of_seq

let to_string bytes =
  let to_hex c =
    let code = Char.code c in
    table.(code lsr 4) lor (table.(code land 0x0f) lsl 8) in
  let buf = Bytes.create (Bytes.length bytes * 3) in
  Bytes.iteri
    (fun idx c ->
      let b = to_hex c in
      Bytes.set_uint16_le buf (idx * 3) b;
      if idx + 1 < Bytes.length bytes then
        Bytes.set buf
          ((idx * 3) + 2)
          (if (idx + 1) mod 16 = 0 then '\n' else ' '))
    bytes;
  Bytes.to_string buf

let pp fmt s = Format.fprintf fmt "%s" (to_string s)

let hex = Alcotest.of_pp pp
