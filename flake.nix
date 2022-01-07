{
  description = "Deku development environment";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.esy-fhs.url = "github:d4hines/esy-fhs";
  inputs.esy-fhs.inputs.nixpkgs.follows = "nixpkgs";
  outputs = { self, nixpkgs, flake-utils, esy-fhs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; }; in
      {
        defaultApp = esy-fhs.lib.makeFHSApp {
          inherit system;
          extraPackages = with pkgs; [
            ligo
          ];
        };
      }
    );
}
