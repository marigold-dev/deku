{
  description = "Deku development environment";

  # Setup trusted binary caches
  nixConfig = {
    trusted-substituters =
      [ "https://cache.nixos.org/" "https://deku.cachix.org" ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ocaml-overlays.url = "github:anmonteiro/nix-overlays";
    ocaml-overlays.inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
    };

    tezos.url = "github:marigold-dev/tezos-nix";
    tezos.inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
    };

    prometheus-web.url = "github:marigold-dev/prometheus-web";
    prometheus-web.inputs = {
      nixpkgs.follows = "nixpkgs";
      ocaml-overlay.follows = "ocaml-overlays";
      flake-utils.follows = "flake-utils";
    };

    dream2nix.url = "github:nix-community/dream2nix";
  };

  outputs =
    { self, nixpkgs, flake-utils, ocaml-overlays, prometheus-web, tezos, dream2nix }:
    let
      dream2nix-lib = dream2nix.lib2.init {
        systems = [
          "x86_64-linux"
          "x86_64-darwin"
          "aarch64-linux"
          "aarch64-darwin"
          "i686-linux"
        ];
        config.projectRoot = ./.;
      };
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = ocaml-overlays.makePkgs {
          inherit system;
          extraOverlays = [
            (import ./nix/overlay.nix)
            prometheus-web.overlays.default
            tezos.overlays.default
          ];
        };

        # Note: In-case this is changed, dream2nix's nodejs setting must also be changed
        nodejs = pkgs.nodejs-16_x;
        pkgs_static = pkgs.pkgsCross.musl64;

        sidechain = (dream2nix-lib.makeFlakeOutputs {
          source = ./.;

          packageOverrides = {
            webpack-cli = {
              remove-webpack-check = {
                patches = [ ./nix/patches/remove-webpack-check.patch ];
              };
            };
          };

          inject = { acorn-import-assertions."1.8.0" = [[ "acorn" "8.7.1" ]]; };

          settings = [{ subsystemInfo.nodejs = "16"; }];
        }).packages.${system}.sidechain;

        npmPackages =
          builtins.attrValues self.packages.${system}.sidechain.dependencies;

        deku = pkgs.callPackage ./nix/deku.nix {
          doCheck = true;
          inherit nodejs sidechain npmPackages;
        };

        deku-static = pkgs_static.callPackage ./nix/deku.nix {
          pkgs = pkgs_static;
          doCheck = true;
          static = true;
          inherit nodejs sidechain npmPackages;
        };
      in {
        devShell = import ./nix/shell.nix {
          inherit pkgs npmPackages sidechain nodejs deku;
        };

        packages = {
          inherit deku deku-static sidechain;
          docker = import ./nix/docker.nix {
            inherit pkgs;
            deku = deku-static;
          };
        };

        apps = {
          deku-cli = {
            type = "app";
            program = "${deku}/bin/deku-cli";
          };
          deku-node = {
            type = "app";
            program = "${deku}/bin/deku-node";
          };
        };
      });
}
