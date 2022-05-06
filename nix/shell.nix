{ pkgs, deku, npmPackages }:
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
      nodejs-16_x
      shellcheck
      tilt
      docker-compose # This is needed by tilt
      jq

      # formatters
      nixfmt
      nodePackages.prettier
      ocamlformat
    ] ++ (with pkgs.ocaml-ng.ocamlPackages_5_00; [
      # OCaml developer tooling
      ocaml
      findlib
      dune_2
      odoc
      ocaml-lsp
      ocamlformat-rpc_0_20_1 # 0.21 is incompatible with latest cmdliner
    ]);
}
