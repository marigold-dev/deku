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
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";

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
  };

  outputs = { self, nixpkgs, flake-utils, nix-npm-buildpackage, ocaml-overlays
    , prometheus-web, tezos }:
    with flake-utils.lib;
    eachSystem defaultSystems (system:
      let
        ligo = (import nixpkgs { inherit system; }).ligo.overrideAttrs
          (_: { meta = { platforms = pkgs.ocaml.meta.platforms; }; });

        pkgs = ocaml-overlays.makePkgs {
          inherit system;
          extraOverlays = [
            (import ./nix/overlay.nix)
            prometheus-web.overlays.default
            tezos.overlays.default
          ];
        };

        pkgs_static = pkgs.pkgsCross.musl64;

        bp =
          pkgs.callPackage nix-npm-buildpackage { nodejs = pkgs.nodejs-12_x; };
        npmPackages = bp.buildNpmPackage {
          src = ./.;
          npmBuild = "echo ok";
        };

        deku = pkgs.callPackage ./nix/deku.nix {
          doCheck = true;
          nodejs = pkgs.nodejs-16_x;
          inherit npmPackages;
        };

        deku-static = pkgs_static.callPackage ./nix/deku.nix {
          pkgs = pkgs_static;
          doCheck = true;
          static = true;
          nodejs = pkgs.nodejs-16_x;
          inherit npmPackages;
        };

        sandbox = pkgs.callPackage ./nix/sandbox.nix {
          inherit deku ligo;
          pkgs = import nixpkgs { inherit system; };
        };
      in {
        devShell =
          import ./nix/shell.nix { inherit pkgs deku ligo npmPackages; };
        packages = {
          inherit deku deku-static npmPackages;
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
          sandbox = {
            type = "app";
            program = "${sandbox}/bin/sandbox.sh";
          };
        };
      }) // {
        hydraJobs = {
          x86_64-linux = self.packages.x86_64-linux;
          aarch64-darwin = {
            # darwin doesn't support static builds and docker
            inherit (self.packages.aarch64-darwin) deku npmPackages;
          };
        };
      };
}
