{
  description = "Nix Flake";

  inputs = {
    nixpkgs.url = "github:EduardoRFS/nix-overlays";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = (nixpkgs.makePkgs {
        inherit system;
        extraOverlays = [
          (import ./nix/overlay.nix)
        ];
      }).extend (self: super: {
        ocamlPackages = super.ocaml-ng.ocamlPackages_5_00;
      }); in
      let deku = pkgs.callPackage ./nix { doCheck = true; }; in
      rec {
        packages = { inherit deku; };
        devShell = import ./nix/shell.nix { inherit pkgs deku; };
      });
}
