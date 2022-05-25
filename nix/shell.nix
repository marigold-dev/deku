{ pkgs, system, deku, ligo, nodejs ? pkgs.nodejs }:
pkgs.mkShell {
  shellHook = ''
    export PATH="$(pwd)/_build/install/default/bin:$PATH"
    # This is a flag picked up by our sandbox.sh that's used
    # to know when to use esy instead of nix.
    # You can turn this off with the command 'unset USE_NIX'.
    export USE_NIX=y
    # Similar to above, but triggers a rebuild before executing
    # the rest of the script.
    export REBUILD=y
  '';
  inputsFrom = [ deku ];
  packages = with pkgs;
    [
      # Make developer life easier
      ## General tooling
      docker
      nodejs
      shellcheck

      docker-compose # This is needed by tilt
      jq

      # Tezos tooling
      ligo

      # Go developer tooling
      go
      gopls
      gore

      # Go developer tooling
      go
      gopls
      gore

      # formatters
      nixfmt
      nodePackages.prettier
      ocamlformat
    ] ++ (pkgs.lib.optional (system != "x86_64-darwin") tilt)
    ++ (with pkgs.ocaml-ng.ocamlPackages_5_00; [
      # OCaml developer tooling
      ocaml
      findlib
      dune_2
      odoc
      ocaml-lsp
      ocamlformat-rpc_0_20_1 # 0.21 is incompatible with latest cmdliner
    ]);
}
