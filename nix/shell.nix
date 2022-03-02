{ pkgs, esy ? pkgs.nodePackages.esy, deku, npmPackages }:

pkgs.mkShell {
  shellHook = ''
    export PATH=${npmPackages}/node_modules/.bin:_build/install/bin:$PATH
  '';

  nativeBuildInputs = (with pkgs; [
    # Make developer life easier
    ## General tooling
    docker
    docker-compose
    nodejs-12_x
    ligo

    # formatters
    nixfmt
    nodePackages.prettier
    ocamlformat_0_19_0
  ]) ++ (with pkgs.ocaml-ng.ocamlPackages_5_00; [
    # OCaml developer tooling
    ocaml
    findlib
    dune_2
    ocaml-lsp
    ocamlformat-rpc
  ]) ++ deku.nativeBuildInputs;

  propagatedBuildInputs = deku.buildInputs;
}