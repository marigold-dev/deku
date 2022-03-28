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

module Prefix = struct
  (* 20 *)
  let contract_hash = "\002\090\121" (* KT1(36) *)

  let deku_contract_hash = "\001\146\006" (* DK1(36) *)

  let ed25519_public_key_hash = "\006\161\159" (* tz1(36) *)

  let secp256k1_public_key_hash = "\006\161\161" (* tz2(36) *)

  let p256_public_key_hash = "\006\161\164" (* tz3(36) *)

  (* 32 *)
  let block_hash = "\001\052" (* B(51) *)

  let operation_hash = "\005\116" (* o(51) *)

  let protocol_hash = "\002\170" (* P(51) *)

  let ed25519_public_key = "\013\015\037\217" (* edpk(54) *)

  let ed25519_seed = "\013\015\058\007" (* edsk(54) *)

  let secp256k1_secret_key = "\017\162\224\201" (* spsk(54) *)

  let p256_secret_key = "\016\081\238\189" (* p2sk(54) *)

  (* 33 *)
  let secp256k1_public_key = "\003\254\226\086" (* sppk(55) *)

  let p256_public_key = "\003\178\139\127" (* p2pk(55) *)

  (* 64 *)
  let ed25519_signature = "\009\245\205\134\018" (* edsig(99) *)

  let secp256k1_signature = "\013\115\101\019\063" (* spsig1(99) *)

  let p256_signature = "\054\240\044\052" (* p2sig(98) *)

  (* 4 *)
  let chain_id = "\087\082\000" (* Net(15) *)
end

let base = 58

let zbase = Z.of_int base

module Alphabet = struct
  type t = {
    encode : string;
    decode : string;
  }

  let make alphabet =
    if String.length alphabet <> base then
      invalid_arg "Base58: invalid alphabet (length)";
    let str = Bytes.make 256 '\255' in
    for i = 0 to String.length alphabet - 1 do
      let char = int_of_char alphabet.[i] in
      if Bytes.get str char <> '\255' then
        Format.kasprintf invalid_arg "Base58: invalid alphabet (dup '%c' %d %d)"
          (char_of_int char)
          (int_of_char @@ Bytes.get str char)
          i;
      Bytes.set str char (char_of_int i)
    done;
    { encode = alphabet; decode = Bytes.to_string str }

  let bitcoin =
    make "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  let ripple = make "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"

  let flickr = make "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"

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
    if s = Z.zero then
      i
    else
      let s, r = Z.div_rem s zbase in
      Bytes.set res i (to_char ~alphabet (Z.to_int r));
      loop s (i - 1) in
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

module TzString = struct
  let fold_left f init s =
    let acc = ref init in
    String.iter (fun c -> acc := f !acc c) s;
    !acc

  let remove_prefix ~prefix s =
    let x = String.length prefix in
    let n = String.length s in
    if n >= x && String.sub s 0 x = prefix then
      Some (String.sub s x (n - x))
    else
      None
end

let count_leading_char s c =
  let len = String.length s in
  let rec loop i =
    if i = len then len else if s.[i] <> c then i else loop (i + 1) in
  loop 0

let of_char ?(alphabet = Alphabet.default) x =
  let pos = alphabet.decode.[int_of_char x] in
  match pos with
  | '\255' -> None
  | _ -> Some (int_of_char pos)

let raw_decode ?(alphabet = Alphabet.default) s =
  TzString.fold_left
    (fun a c ->
      match (a, of_char ~alphabet c) with
      | Some a, Some i -> Some Z.(add (of_int i) (mul a zbase))
      | _ -> None)
    (Some Z.zero) s
  |> Option.map (fun res ->
         let res = Z.to_bits res in
         let res_tzeros = count_trailing_char res '\000' in
         let len = String.length res - res_tzeros in
         let zeros = count_leading_char s alphabet.encode.[0] in
         String.make zeros '\000' ^ String.init len (fun i -> res.[len - i - 1]))

let safe_decode ?alphabet s =
  Option.bind (raw_decode ?alphabet s) (fun s ->
      let len = String.length s in
      if len < 4 then
        None
      else
        (* only if the string is long enough to extract a checksum do we check it *)
        let msg = String.sub s 0 (len - 4) in
        let msg_hash = String.sub s (len - 4) 4 in
        if msg_hash <> checksum msg then None else Some msg)

let simple_decode ?alphabet ~prefix ~of_raw s =
  let ( >?? ) = Option.bind in
  safe_decode ?alphabet s >?? TzString.remove_prefix ~prefix >?? of_raw
