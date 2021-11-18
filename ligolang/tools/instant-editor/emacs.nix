{ sources ? import ./nix/sources.nix, pkgs ? import sources.nixpkgs { } }:

let
  init-el = pkgs.writeText "init.el" ''
    (setq use-package-always-ensure nil) ;; Dependencies are installed via nix

    (eval-when-compile
      (require 'use-package))

    (use-package company)

    (use-package nix-mode
      :mode "\\.nix\\'")

    ${builtins.readFile ./ocaml-config.el}

    (setq inhibit-startup-message t)

    (message "Welcome to LIGO Instant Emacs")
  '';
  emacs = pkgs.writeShellScriptBin "emacs" ''
    ${
      pkgs.emacsWithPackages (ps:
        with ps; [
          use-package
          tuareg
          company
          nix-mode
          merlin
          utop
          ocp-indent
        ]
        ++ (with ps.melpaStablePackages ; [iedit]))
    }/bin/emacs \
      -l "${init-el}" \
      -q \
      "$@"
  '';

in {
  buildInputs = [
    pkgs.niv
    pkgs.opam
    emacs
    pkgs.lorri
    pkgs.tmux
    pkgs.direnv
    pkgs.ocamlPackages.ocp-indent
    pkgs.ocamlPackages.merlin
    pkgs.git
  ];
  shellHook = ''eval "$(${pkgs.direnv}/bin/direnv hook bash)"'';
}
