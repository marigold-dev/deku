{
  description = "Deku development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ocaml-overlays.url = "github:anmonteiro/nix-overlays";
    ocaml-overlays.inputs.nixpkgs.follows = "nixpkgs";

    esy-fhs.url = "github:d4hines/esy-fhs";
    esy-fhs.inputs.nixpkgs.follows = "nixpkgs";
    esy-fhs.inputs.anmonteiro.follows = "ocaml-overlay";
    esy-fhs.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ocaml-overlays, esy-fhs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ ocaml-overlays.overlay ];
        };

        esy = esy-fhs.packages.${system}.esy;
      in {
        devShell = (pkgs.mkShell {
          shellHook = ''
            npm install
            export PATH="node_modules/.bin:_build/default/src/bin:$PATH"
          '';
          buildInputs = with pkgs; [
            # Make developer life easier
            ## General tooling
            docker
            docker-compose

            # OCaml developer tooling
            esy

            # Nix files formatter
            nixfmt
          ];

          packages = with pkgs.ocaml-ng.ocamlPackages_multicore; [
            ocaml
            dune_2
            findlib
            ocaml-lsp
            ocamlformat-rpc
            pkgs.nodejs
            pkgs.nodePackages.npm
          ];

          propagatedBuildInputs = with pkgs.ocaml-ng.ocamlPackages_multicore; [
            ppx_deriving
            ppx_deriving_yojson
            lwt
            dream
            mirage-crypto
            mirage-crypto-pk
            mirage-crypto-rng
            mirage-crypto-ec
            piaf
            mrmime
            hex
            tezos-micheline
            digestif
            cmdliner
            ppx_blob
            secp256k1-internal
            bigstring
            domainslib
            utop
          ];
        });
      });
}
