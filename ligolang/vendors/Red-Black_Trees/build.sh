#!/bin/sh
set -x
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c RedBlack.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c PolyMap.mli
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c RedBlack.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c PolyMapMain.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c PolyMap.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c PolyMapMain.ml
ocamlfind ocamlopt -o PolyMapMain.opt RedBlack.cmx PolyMap.cmx PolyMapMain.cmx
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c PolySet.mli
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c PolySet.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c PolySetMain.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c PolySetMain.ml
ocamlfind ocamlopt -o PolySetMain.opt RedBlack.cmx PolySet.cmx PolySetMain.cmx
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c RedBlackMain.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c RedBlackMain.ml
ocamlfind ocamlopt -o RedBlackMain.opt RedBlack.cmx RedBlackMain.cmx
