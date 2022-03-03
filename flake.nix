{
  description = "Deku development environment";

  # Setup trusted binary caches
  nixConfig = {
    trusted-substituters = [
      "https://cache.nixos.org/"
      "https://deku.cachix.org"
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # Use this branch until we can merge it in the overlays
    ocaml-overlays.url = "github:anmonteiro/nix-overlays";
    ocaml-overlays.inputs.nixpkgs.follows = "nixpkgs";

    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
  };

  outputs = { self, nixpkgs, flake-utils, nix-npm-buildpackage, ocaml-overlays }:
    flake-utils.lib.eachDefaultSystem (system:
      let        
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ ocaml-overlays.overlay ];
        };

        bp = pkgs.callPackage nix-npm-buildpackage { nodejs = pkgs.nodejs-12_x; };
        npmPackages = bp.buildNpmPackage { src = ./.; npmBuild = "echo ok"; };

        deku = pkgs.callPackage ./nix/deku.nix {
          doCheck = true;
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_00;
          nodejs = pkgs.nodejs-12_x;
          inherit npmPackages;
        };
      in
      {
        devShell = import ./nix/shell.nix {inherit pkgs deku npmPackages; };
        packages = {
          inherit deku;
        };
      });
}
