{ pkgs, esy ? pkgs.nodePackages.esy, deku, npmPackages }:
pkgs.mkShell {
  shellHook = ''
    export NODE_PATH=${npmPackages}/node_modules
    export PATH=${npmPackages}/node_modules/.bin:_build/install/default/bin:$PATH
  '';
  inputsFrom = [ deku ];
  packages = with pkgs; [
    # Make developer life easier
    ## General tooling
    docker
    nodejs-17_x

    # formatters
    nixfmt
    nodePackages.prettier
    ocamlformat
  ] ++ (with pkgs.ocamlPackages;
    [
      # OCaml developer tooling
      ocaml
      findlib
      dune_2
      ocaml-lsp
      ocamlformat-rpc
    ]);
}
