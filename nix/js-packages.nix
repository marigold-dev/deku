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
        decookie-vm =
          let script = pkgs.writeScriptBin "decookie-vm" ''
            export NODE_PATH="${self'.packages.decookie-vm}/lib/node_modules/decookie-vm/node_modules"
            ${nodejs}/bin/node ${self'.packages.decookie-vm}/lib/node_modules/decookie-vm/lib/src/index.js "$@"
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
