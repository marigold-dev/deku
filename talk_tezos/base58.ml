(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let base = 58

let zbase = Z.of_int base

module Alphabet = struct
  type t = {encode : string}

  let make alphabet =
    if String.length alphabet <> base then
      invalid_arg "Base58: invalid alphabet (length)" ;
    let str = Bytes.make 256 '\255' in
    for i = 0 to String.length alphabet - 1 do
      let char = int_of_char alphabet.[i] in
      if Bytes.get str char <> '\255' then
        Format.kasprintf
          invalid_arg
          "Base58: invalid alphabet (dup '%c' %d %d)"
          (char_of_int char)
          (int_of_char @@ Bytes.get str char)
          i ;
      Bytes.set str char (char_of_int i)
    done ;
    {encode = alphabet}

  let bitcoin =
    make "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  let ripple =
    make "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"

  let flickr =
    make "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"

  let default = bitcoin
end

let count_trailing_char s c =
  let len = String.length s in
  let rec loop i =
    if i < 0 then len else if s.[i] <> c then len - i - 1 else loop (i - 1)
  in
  loop (len - 1)

let to_char ?(alphabet = Alphabet.default) x = alphabet.encode.[x]

let raw_encode ?(alphabet = Alphabet.default) s =
  let len = String.length s in
  let s = String.init len (fun i -> s.[len - i - 1]) in
  let zero = alphabet.encode.[0] in
  let zeros = count_trailing_char s '\000' in
  let res_len = ((len * 8) + 4) / 5 in
  let res = Bytes.make res_len '\000' in
  let s = Z.of_bits s in
  let rec loop s i =
    if s = Z.zero then i
    else
      let (s, r) = Z.div_rem s zbase in
      Bytes.set res i (to_char ~alphabet (Z.to_int r)) ;
      loop s (i - 1)
  in
  let i = loop s (res_len - 1) in
  let ress = Bytes.sub_string res (i + 1) (res_len - i - 1) in
  String.make zeros zero ^ ress

let checksum s =
  let hash = Mirage_crypto.Hash.SHA256.(digest (digest (Cstruct.of_string s))) in
  String.sub (Cstruct.to_string hash) 0 4

(* Append a 4-bytes cryptographic checksum before encoding string s *)
let safe_encode ?alphabet s = raw_encode ?alphabet (s ^ checksum s)

let simple_encode ?alphabet ~prefix ~to_raw d =
  safe_encode ?alphabet (prefix ^ to_raw d)

module Prefix = struct
  (* 20 *)
  let ed25519_public_key_hash = "\006\161\159" (* tz1(36) *)

  (* 32 *)
  let ed25519_public_key = "\013\015\037\217" (* edpk(54) *)

  (* 64 *)
  let ed25519_signature = "\009\245\205\134\018" (* edsig(99) *)
end
