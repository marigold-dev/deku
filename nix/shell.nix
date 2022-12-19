{
  pkgs,
  deku,
  tuna,
  deploy-rs,
}:
with pkgs;
with ocamlPackages;
  mkShell {
    inputsFrom = [deku tuna];
    packages = [
      # Formatters
      alejandra
      ocamlformat
      nodePackages.prettier

      # Typescript for decookie
      nodePackages.typescript

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
