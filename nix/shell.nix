{ pkgs, deku, ligo, deploy-rs }:

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

    # deployment
    deploy-rs

    # helpful tooling
    bc
    sqlite
    termdbms
  ];
}
