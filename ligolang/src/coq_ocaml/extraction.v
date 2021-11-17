(* ZArith stuff below was stolen from Mi-cho-coq:
   https://gitlab.com/nomadic-labs/mi-cho-coq/-/blob/eb5a0b469c45472afa87335abee5c1644eb10349/src/michocoq/extraction/extraction.v
   *)

(* Open Source License *)
(* Copyright (c) 2019 Nomadic Labs. <contact@nomadic-labs.com> *)

(* Permission is hereby granted, free of charge, to any person obtaining a *)
(* copy of this software and associated documentation files (the "Software"), *)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense, *)
(* and/or sell copies of the Software, and to permit persons to whom the *)
(* Software is furnished to do so, subject to the following conditions: *)

(* The above copyright notice and this permission notice shall be included *)
(* in all copies or substantial portions of the Software. *)

From ligo_coq Require Import compiler co_de_bruijn.
From Coq Require Extraction.
Require ExtrOcamlBasic.
Require ExtrOcamlNativeString.
Require Import ZArith NArith.

(* Mapping Z to the OCaml library Zarith. *)
Extract Inductive positive =>
"Zarith.t"
  [ "(fun x -> Zarith.add Zarith.one (Zarith.mul (Zarith.add Zarith.one Zarith.one) x))" "(fun x -> Zarith.mul (Zarith.add Zarith.one Zarith.one) x)" "Zarith.one" ]
  "(fun b1 b2 b3 x -> Zarith.(if x = one then b3 () else let (q, r) = ediv_rem x (of_int 2) in if r = zero then b2 q else b1 q))".

Extract Inductive Z =>
"Zarith.t"
  [ "Zarith.zero" "" "Zarith.neg" ]
  "(fun b1 b2 b3 x -> Zarith.(if x > zero then b2 x else if x < zero then b3 x else b1 ()))".

Extract Inductive N => "Zarith.t"
 [ "Zarith.zero" "" ] "(fun b1 b2 x -> Zarith.(if x > zero then b2 x else b1 ()))".

(* TODO *)
Local Set Warnings "-extraction-logical-axiom".

Separate Extraction compiler union.
