{ pkgs, deku, nodejs, npm-deps, npmPackages, patched-webpack }:
pkgs.mkShell {
  shellHook = ''
    export PATH=_build/install/default/bin:$PATH

    # This is a flag picked up by our sandbox.sh that's used
    # to know when to use esy instead of nix.
    # You can turn this off with the command 'unset USE_NIX'.
    export USE_NIX=y
  '';
  inputsFrom = [ deku ];
  packages = with pkgs;
    [
      patched-webpack

      # Make developer life easier
      ## General tooling
      docker
      nodejs
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
    ]) ++ npmPackages;
}
