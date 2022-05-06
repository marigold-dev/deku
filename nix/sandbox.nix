{ pkgs, deku, }:

let
  ligo = pkgs.ligo.overrideAttrs (o: {
    meta = { platforms = pkgs.ocaml.meta.platforms; };
  });
in

pkgs.writeShellApplication {
  name = "sandbox.sh";
  runtimeInputs = with pkgs; [ deku ligo jq curl docker ];
  text = builtins.readFile ../sandbox.sh;
}
