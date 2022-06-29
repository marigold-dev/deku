{ stdenv, lib, fetchFromGitHub, ocaml, findlib, ocamlbuild, buildDunePackage }:


  buildDunePackage {
    pname = "wasm";
    version = "2.0.0";

    src = fetchFromGitHub {
      owner = "zett98";
      repo = "spec";
      rev = "c7f05aaf96c9e13de3f3f822cd679c3fd32a108d";
      sha256 = "sha256-pyRFXoxZZ/x8JS0+T2zTHApFjjdHKeDlcLQ72B/gj70=";
    };

    strictDeps = true;

    # x86_64-unknown-linux-musl-ld: -r and -pie may not be used together
    hardeningDisable = lib.optional stdenv.hostPlatform.isStatic "pie";

    meta = {
      description = "An executable and OCaml library to run, read and write Web Assembly (wasm) files and manipulate their AST";
      license = lib.licenses.asl20;
      maintainers = [ lib.maintainers.vbgl ];
      homepage = "https://github.com/WebAssembly/spec/tree/master/interpreter";
      inherit (ocaml.meta) platforms;
    };
  }
