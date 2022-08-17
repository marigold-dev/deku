{ pkgs }:

pkgs.writeShellApplication {
  name = "format.sh";
  runtimeInputs = with pkgs; [
    treefmt
    nixfmt
    ocamlformat_0_21_0
    ocaml
    dune_3
    nodePackages.prettier
  ];
  text = ''treefmt "$@"'';
}
