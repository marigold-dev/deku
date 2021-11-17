# nixpkgs extended with all the overlays for LIGO
{ sources ? import ./sources.nix }:
let
  ocaml-overlay = import ./ocaml-overlay.nix { inherit sources; };
  static-overlay = import ./static-overlay.nix;
  mac-overlay = import ./mac-overlay.nix;
  nodejs-overlay = import ./nodejs-overlay.nix;
  nix-npm-buildpackage = pkgs.callPackage sources.nix-npm-buildpackage { };

  pkgs = import sources.nixpkgs {
    overlays = [ ocaml-overlay nodejs-overlay ];
  };

  # Takes $pkg/ligo and creates a new package with $pkg/bin/ligo
  separateBinary = pkg:
    pkgs.runCommandNoCC "${pkg.name}-bin" { }
      "mkdir -p $out/bin; cp -Lr ${pkg}/ligo $out/bin";

  compressBinaries = pkg:
    pkgs.runCommandNoCC "${pkg.name}-compressed" { nativeBuildInputs = [ pkgs.upx ]; }
      "cp -Lr ${pkg} $out; chmod -R +w $out/bin; upx $out/bin/*";


  tmp = pkgs.runCommandNoCC "tmpdir" { } "mkdir -p $out/tmp";

in pkgs.extend (self: super: {
  inherit (self.ocamlPackages) ligo ligo-out ligo-tests ligo-doc ligo-coverage;
  ligo-bin = separateBinary self.ligo-out.bin;
  ligo-docker = self.callPackage ./docker.nix { ligo = self.ligo-bin; };
  ligo-docker-large = self.callPackage ./docker.nix {
    ligo = self.ligo-bin;
    extraContents = [ self.coreutils ];
  };
  ligo-deb = self.callPackage ./packageDeb.nix { };
  ligo-editor = self.callPackage ./ligo-editor.nix { inherit sources; };
  ligo-editor-docker = self.callPackage ./docker.nix {
    ligo = self.ligo-editor;
    name = "ligo-editor";
    extraContents = [ tmp ];
  };
  ligo-website = self.callPackage ./ligo-website.nix {
    inherit (nix-npm-buildpackage) buildNpmPackage;
  };
  ligo-changelog = self.callPackage ./changelog.nix { };
  ligo-static = compressBinaries self.pkgsMusl.ligo-bin;
  pkgsMusl = super.pkgsMusl.extend (static-overlay self);
})
