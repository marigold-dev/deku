{
  pkgs,
  deku,
  ligo,
  tuna,
  deploy-rs,
}:
with pkgs;
with ocamlPackages;
  mkShell {
    inputsFrom = [deku tuna];
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
