{
  description = "Nix Flake";

  nixConfig = {
    extra-substituters = "https://anmonteiro.nix-cache.workers.dev";
    extra-trusted-public-keys = "ocaml.nix-cache.com-1:/xI2h2+56rwFfKyyFVbkJSeGqSIYMC/Je+7XXqGKDIY=";
  };

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";

    dream2nix.url = "github:nix-community/dream2nix";
    dream2nix.inputs.nixpkgs.follows = "nixpkgs";

    tezos.url = "github:marigold-dev/tezos-nix";
    tezos.inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter, dream2nix, tezos }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (nixpkgs.makePkgs {
          inherit system;
          extraOverlays = [
            tezos.overlays.default
            (import ./nix/overlay.nix)
            (final: prev: {
              ocamlPackages = prev.ocaml-ng.ocamlPackages_5_00;
            })
          ];
        });
        dream2nix-lib = dream2nix.lib.init {
          inherit pkgs;
          config.projectRoot = ./.;
        };
        nodejs = pkgs.nodejs-16_x;
        npmPackages = import ./nix/npm.nix {
          inherit system dream2nix-lib nix-filter nodejs;
        };

        deku = pkgs.callPackage ./nix {
          inherit nodejs npmPackages;
          doCheck = true;
        };

        ligo = pkgs.callPackage ./nix/ligo.nix { };
      in
      rec {
        packages = { default = deku; };
        apps = {
          node = {
            type = "app";
            program = "${deku}/bin/deku-node";
          };
          bootstrap = {
            type = "app";
            program = "${deku}/bin/deku-bootstrap";
          };
          benchmark = {
            type = "app";
            program = "${deku}/bin/deku-benchmark";
          };
        };
        devShells.default = import ./nix/shell.nix { inherit pkgs deku ligo; };
        nixosModules = {
          deku-node = import ./nix/service.nix { inherit deku; };
        };
      });
}
