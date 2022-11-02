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
      tuna = pkgs.ocamlPackages.callPackage ./tuna.nix {inherit (inputs) nix-filter;};
    };
  };
}
