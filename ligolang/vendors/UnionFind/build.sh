#!/bin/sh
set -x
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Partition.mli
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Partition0.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Partition2.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Partition1.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Partition3.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Partition1.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Partition3.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Partition0.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Partition2.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c PartitionMain.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c PartitionMain.ml
ocamlfind ocamlopt -o PartitionMain.opt Partition0.cmx Partition1.cmx Partition2.cmx Partition3.cmx PartitionMain.cmx
