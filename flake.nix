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
    ocaml-overlays.inputs.nixpkgs.follows = "nixpkgs";

    prometheus-web.url =
      "github:marigold-dev/prometheus-web/ulrikstrid/nix-stuff";
    prometheus-web.inputs.nixpkgs.follows = "nixpkgs";
    prometheus-web.inputs.ocaml-overlay.follows = "ocaml-overlays";

    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    tezos.url = "github:marigold-dev/tezos-nix";
    tezos.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , nix-npm-buildpackage
    , ocaml-overlays
    , prometheus-web
    , tezos
    }:
      with flake-utils.lib;
      eachSystem defaultSystems (system:
      let
        ligo = (import nixpkgs { inherit system; }).ligo;
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            ocaml-overlays.overlay
            (import ./nix/overlay.nix)
            prometheus-web.overlay
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
          nodejs = pkgs.nodejs-12_x;
          inherit npmPackages;
        };

        deku-static = pkgs_static.callPackage ./nix/deku.nix {
          pkgs = pkgs_static;
          doCheck = true;
          static = true;
          nodejs = pkgs.nodejs-12_x;
          inherit npmPackages;
        };

        sandbox = pkgs.callPackage ./nix/sandbox.nix {
          inherit pkgs deku ligo;
        };
      in
      {
        devShell = import ./nix/shell.nix { inherit pkgs deku npmPackages sandbox; };
        packages = {
          inherit deku deku-static sandbox;
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
      });
}
