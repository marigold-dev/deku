{
  self,
  inputs,
  ...
}: {
  perSystem = {
    config,
    self',
    system,
    pkgs,
    ...
  }: {
    packages = {
      vm_library = pkgs.callPackage ./vm_library.nix {inherit (inputs) nix-filter;};
      tuna = pkgs.ocamlPackages.callPackage ./tuna.nix {inherit (inputs) nix-filter;};
    };
    apps = {
      wasm-vm = let
        script = pkgs.writeScriptBin "wasm-vm" ''
          RUST_LOG=info ${self'.packages.vm_library}/bin/vm_library "$@"
        '';
      in {
        type = "app";
        program = "${script}/bin/wasm-vm";
      };
    };
  };
}
