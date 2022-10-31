type codepoint = Uchar.t [@@deriving equal, compare]
type t = String.t [@@deriving equal, compare]

exception Utf8

let encode x = x

module Deser = struct
  type t = { s : string; len : int; mutable i : int }

  let make ?(idx = 0) (s : string) : t = { s; i = idx; len = String.length s }
end

let next_ (type a) (st : Deser.t) ~(yield : codepoint -> a) ~(stop : unit -> a)
    () : a =
  let open Deser in
  let malformed st = raise Utf8 in
  let read_multi ?(overlong = 0) n_bytes acc =
    let rec aux j acc =
      let c = Char.code st.s.[st.i + j] in
      if c lsr 6 <> 0b10 then malformed st;
      if j = 1 && overlong <> 0 && c land 0b111111 < overlong then malformed st;
      let next = (acc lsl 6) lor (c land 0b111111) in
      if j = n_bytes then
        if Uchar.is_valid next then (
          st.i <- st.i + j + 1;
          yield (Uchar.of_int next))
        else malformed st
      else aux (j + 1) next
    in
    assert (n_bytes >= 1);
    if st.i + n_bytes < st.len then aux 1 acc else malformed st
  in
  if st.i >= st.len then stop ()
  else
    let c = st.s.[st.i] in

    match c with
    | '\000' .. '\127' ->
        st.i <- 1 + st.i;
        yield (Uchar.of_char c)
    | '\194' .. '\223' -> read_multi 1 (Char.code c land 0b11111)
    | '\225' .. '\239' -> read_multi 2 (Char.code c land 0b1111)
    | '\241' .. '\244' -> read_multi 3 (Char.code c land 0b111)
    | '\224' -> read_multi ~overlong:0b00100000 2 (Char.code c land 0b1111)
    | '\240' -> read_multi ~overlong:0b00010000 3 (Char.code c land 0b111)
    | '\128' .. '\193' | '\245' .. '\255' -> malformed st

let validate_exn (s : string) : bool =
  let exception Stop in
  try
    let st = Deser.make s in
    while true do
      next_ st ~yield:(fun _ -> ()) ~stop:(fun () -> raise Stop) ()
    done;
    assert false
  with
  | Utf8 -> raise Utf8
  | Failure _ -> raise Utf8
  | Stop -> true

let decode s = if validate_exn s then s else raise Utf8
