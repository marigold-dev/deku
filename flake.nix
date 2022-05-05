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
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, ocaml-overlays, prometheus-web, tezos
    , dream2nix, nix-filter }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      dream2nix-lib = dream2nix.lib2.init {
        systems = supportedSystems;
        config.projectRoot = ./.;
      };
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = ocaml-overlays.makePkgs {
          inherit system;
          extraOverlays = [
            (import ./nix/overlay.nix)
            prometheus-web.overlays.default
            tezos.overlays.default
          ];
        };

        pkgs_static = pkgs.pkgsCross.musl64;

        # Note: In-case this is changed, dream2nix's nodejs setting must also be changed
        nodejs = pkgs.nodejs-16_x;

        npm-deps = self.packages.${system}.npm-deps;

        npmPackages = builtins.attrValues npm-deps.dependencies;

        deku = pkgs.callPackage ./nix/deku.nix {
          doCheck = true;
          inherit nodejs npm-deps npmPackages;
        };

        deku-static = pkgs_static.callPackage ./nix/deku.nix {
          pkgs = pkgs_static;
          doCheck = true;
          static = true;
          inherit nodejs npm-deps npmPackages;
        };
      in {
        devShell = import ./nix/shell.nix {
          inherit pkgs npm-deps npmPackages nodejs deku;
        };

        packages = {
          inherit deku deku-static;

          npm-deps = (dream2nix-lib.makeFlakeOutputs {
            source = nix-filter.lib.filter {
              root = ./.;
              include = [ ./package-lock.json ./package.json ];
            };

            packageOverrides = {
              webpack-cli = {
                remove-webpack-check = {
                  patches = [ ./nix/patches/remove-webpack-check.patch ];
                };
              };
            };

            inject = {
              acorn-import-assertions."1.8.0" = [[ "acorn" "8.7.1" ]];
            };

            settings = [{ subsystemInfo.nodejs = "16"; }];
          }).packages.${system}.sidechain;

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
