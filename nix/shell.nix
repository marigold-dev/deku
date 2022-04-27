{ pkgs, esy ? pkgs.nodePackages.esy, deku, npmPackages, sandbox }:
pkgs.mkShell {
  shellHook = ''
    export NODE_PATH=${npmPackages}/node_modules
    export PATH=${npmPackages}/node_modules/.bin:_build/install/default/bin:$PATH

    # This is a flag picked up by our sandbox.sh that's used
    # to know when to rebuild the binaries.
    export REBUILD=y
  '';
  inputsFrom = [ deku ];
  packages = with pkgs;
    [
      # Make developer life easier
      ## General tooling
      docker
      nodejs-17_x
      shellcheck
      tilt
      docker-compose # This is needed by tilt

      # formatters
      nixfmt
      nodePackages.prettier
      ocamlformat
    ] ++ (with pkgs.ocaml-ng.ocamlPackages_5_00; [
      # OCaml developer tooling
      ocaml
      findlib
      dune_2
      ocaml-lsp
      ocamlformat-rpc
    ]);
}
