{ pkgs, deku }:

with pkgs; with ocamlPackages; mkShell {
  inputsFrom = [ deku ];
  packages = [
    # Formatters
    nixfmt
    ocamlformat

    # Tezos tooling
    ligo 

    # OCaml developer tooling
    ocaml
    dune_3
    ocaml-lsp
    ocamlformat-rpc
    utop

    # helpful tooling
    bc
    sqlite
    termdbms
  ];
}
