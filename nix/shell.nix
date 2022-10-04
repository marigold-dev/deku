{ pkgs, deku, ligo, deploy-rs }:

with pkgs; with ocamlPackages; mkShell {
  shellHook = ''
      export PATH="$(pwd)/_build/install/default/bin:$PATH"
  '';
  inputsFrom = [ deku ];
  packages = [
    # Formatters
    nixfmt
    ocamlformat

    # Typescript for decookie
    nodePackages.typescript

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
