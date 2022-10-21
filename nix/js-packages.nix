{ self, inputs, ... }:

{
  perSystem =
    { config
    , self'
    , system
    , pkgs
    , dream2nix-lib
    , nodejs
    , ...
    }: {
      packages = builtins.removeAttrs (dream2nix-lib.makeOutputs {
        source = inputs.nix-filter.lib.filter {
          root = ../.;
          include =
            [ "package.json" "yarn.lock" ]
            ++
            (builtins.fromJSON (builtins.readFile ../package.json)).workspaces;
        };

        settings =
          [{ subsystemInfo.nodejs = (builtins.substring 0 2 nodejs.version); }];
      }).packages [ "default" ""];
      apps = {
        decookies-vm =
          let script = pkgs.writeScriptBin "decookies-vm" ''
            export NODE_PATH="${self'.packages.decookies-vm}/lib/node_modules/decookies-vm/node_modules"
            ${nodejs}/bin/node ${self'.packages.decookies-vm}/lib/node_modules/decookies-vm/lib/src/index.js "$@"
          '';
          in
          {
            type = "app";
            # FIXME: should we standardize the version of node used with an overlay?
            program = "${script}/bin/decookies-vm";
          };
      };
    };
}
