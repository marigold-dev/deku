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

(** {1 Prefixed Base58Check encodings} *)

module Prefix : sig
  val contract_hash : string

  val operation_hash : string

  val ed25519_public_key_hash : string
  val secp256k1_public_key_hash: string
  val p256_public_key_hash: string


  val ed25519_public_key : string
  val secp256k1_public_key: string
  val p256_public_key: string


  val ed25519_seed : string

  val secp256k1_secret_key: string
  val p256_secret_key: string

  val ed25519_signature : string
  val secp256k1_signature: string
  val p256_signature: string

end

module Alphabet : sig
  type t

  val bitcoin : t

  val ripple : t

  val flickr : t
end

(** Encoder for a given kind of data. *)
val simple_encode :
  ?alphabet:Alphabet.t ->
  prefix:string ->
  to_raw:('a -> string) ->
  'a ->
  string

(** Decoder for a given kind of data. It returns [None] when
  the decoded data does not start with the expected prefix. *)
val simple_decode :
  ?alphabet:Alphabet.t ->
  prefix:string ->
  of_raw:(string -> 'a option) ->
  string ->
  'a option
