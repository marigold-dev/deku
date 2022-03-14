{ pkgs, esy ? pkgs.nodePackages.esy, deku, npmPackages }:

pkgs.mkShell {
  shellHook = ''
    export NODE_PATH=${npmPackages}/node_modules
    export PATH=${npmPackages}/node_modules/.bin:_build/install/default/bin:$PATH
  '';

  nativeBuildInputs = (with pkgs; [
    # Make developer life easier
    ## General tooling
    docker
    docker-compose
    nodejs-12_x

    # formatters
    nixfmt
    nodePackages.prettier
  ]) ++ (with pkgs.ocamlPackages; [
    # OCaml developer tooling
    ocaml
    findlib
    dune_2
    ocaml-lsp
    ocamlformat-rpc
  ]) ++ deku.nativeBuildInputs;

  propagatedBuildInputs = deku.buildInputs;
}
