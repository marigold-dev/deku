{ self, inputs, ...}:

{
  perSystem = {
    config, self', system, pkgs, dream2nix-lib,nodejs, ...
  }: {
    packages = {
      decookie-vm = pkgs.callPackage ./decookie-vm.nix { inherit dream2nix-lib; inherit (inputs) nix-filter; };
    };
    apps = {
      decookie-vm =
        let script = pkgs.writeScriptBin "decookie-vm" ''
          export NODE_PATH="${self'.decookie-vm}/lib/node_modules/cookie-game/node_modules"
          ${nodejs}/bin/node ${self'.decookie-vm}/lib/node_modules/cookie-game/lib/src/index.js "$@"
        '';
        in
        {
          type = "app";
          # FIXME: should we standardize the version of node used with an overlay?
          program = "${script}/bin/decookie-vm";
        };
    };
  };
}