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
  };

  outputs = { self, nixpkgs, flake-utils, nix-npm-buildpackage, ocaml-overlays
    , prometheus-web }:
    with flake-utils.lib;
    eachSystem defaultSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            ocaml-overlays.overlay
            (import ./nix/overlay.nix)
            prometheus-web.overlay
          ];
        };

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
      in {
        devShell = import ./nix/shell.nix { inherit pkgs deku npmPackages; };
        packages = { inherit deku; };

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
