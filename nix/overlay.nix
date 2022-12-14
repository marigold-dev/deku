self: super:
with super; {
  # ocaml-ng = builtins.mapAttrs (_: ocamlVersion: ocamlVersion) super.ocaml-ng;
  # TODO: this is clearly not right, I should be overriding only 4_14
  ocaml-ng =
    ocaml-ng
    // (with ocaml-ng; {
      ocamlPackages_5_00 =
        ocamlPackages_5_00.overrideScope'
        (oself: super: {
          dune-rpc = super.dune-rpc.overrideAttrs (_: {
            src = fetchFromGitHub {
              owner = "ocaml";
              repo = "dune";
              rev = "3df932f7f91ea68c3fee789f133b4aa8f9bea807";
              sha256 = "m0HDamHAyOFQlh6P4vbbe8UhARZ4NjwbANInnJ4OZ6U=";
              fetchSubmodules = true;
            };
          });
          dune = super.dune.overrideAttrs (_: {
            src = fetchFromGitHub {
              owner = "ocaml";
              repo = "dune";
              rev = "3df932f7f91ea68c3fee789f133b4aa8f9bea807";
              sha256 = "m0HDamHAyOFQlh6P4vbbe8UhARZ4NjwbANInnJ4OZ6U=";
              fetchSubmodules = true;
            };
          });
          ocaml-lsp = super.ocaml-lsp.overrideAttrs (_: {
            src = fetchFromGitHub {
              owner = "EduardoRFS";
              repo = "ocaml-lsp";
              rev = "72202cd2bb9ff65845b623d20f513f6ed5b82ab8";
              sha256 = "49E7L50i9RZTrQDPmdqyeOaBSXjRo/ijjrsj9oztduM=";
              fetchSubmodules = true;
            };
          });
          jsonrpc = super.jsonrpc.overrideAttrs (_: {
            src = fetchFromGitHub {
              owner = "EduardoRFS";
              repo = "ocaml-lsp";
              rev = "72202cd2bb9ff65845b623d20f513f6ed5b82ab8";
              sha256 = "49E7L50i9RZTrQDPmdqyeOaBSXjRo/ijjrsj9oztduM=";
              fetchSubmodules = true;
            };
          });

          ringo = oself.buildDunePackage rec {
            pname = "ringo";
            version = "1.0.0";

            src = fetchFromGitLab {
              owner = "nomadic-labs";
              repo = "ringo";
              rev = "v${version}";
              sha256 = "9HW3M27BxrEPbF8cMHwzP8FmJduUInpQQAE2672LOuU=";
            };
          };

          aches = oself.buildDunePackage rec {
            pname = "aches";
            inherit (super.ringo) src version;
            propagatedBuildInputs = [super.ringo];
          };

          aches-lwt = oself.buildDunePackage rec {
            pname = "aches-lwt";
            inherit (super.ringo) src version;

            propagatedBuildInputs = [
              super.aches
              lwt
            ];
          };

          resto = super.resto.overrideAttrs (_: rec {
            version = "1.0";
            src = fetchFromGitLab {
              owner = "nomadic-labs";
              repo = "resto";
              rev = "v${version}";
              sha256 = "sha256-DIm7fmISsCgRDi4p3NsUk7Cvs/dHpIKMdAOVdYLX2mc=";
            };
          });

          tezos-proxy = super.tezos-proxy.overrideAttrs (self: {
            propagatedBuildInputs = with super; with lib.lists;
              [aches aches-lwt] ++ (remove ringo-lwt (remove ringo self.propagatedBuildInputs));
          });

          tezos-store = super.tezos-store.overrideAttrs (self: {
            propagatedBuildInputs = with super; with lib.lists;
              [aches aches-lwt] ++ (remove ringo-lwt (remove ringo self.propagatedBuildInputs));
          });

          tezos-stdlib = super.tezos-stdlib.overrideAttrs (self: {
            propagatedBuildInputs = with super; with lib.lists;
              [aches] ++ (remove ringo-lwt (remove ringo self.propagatedBuildInputs));
          });

          tezos-protocol-environment = super.tezos-protocol-environment.overrideAttrs (self: {
            propagatedBuildInputs = with super; with lib.lists;
              [aches aches-lwt] ++ (remove ringo-lwt (remove ringo self.propagatedBuildInputs));
          });

          tezos-micheline = super.tezos-micheline.overrideAttrs (_: {
            doCheck = false;
          });

          routes = super.routes.overrideAttrs (_: {
            src = fetchFromGitHub {
              owner = "anuragsoni";
              repo = "routes";
              rev = "3cf574ebed7b60366fa8ddcbe8f7c2c5f83c678f";
              sha256 = "sha256-Q7ZWcCIiA0K+m8DvnXBhQlVKFmMly1D+Fz+hmLhE2WU=";
            };
          });

          caqti-eio = oself.buildDunePackage {
            pname = "caqti-eio";
            version = "n/a";
            src = builtins.fetchurl {
              url = https://github.com/anmonteiro/caqti-eio/archive/c709dad.tar.gz;
              sha256 = "0mmjms378akcs7lifpz3s82hw7g6sdxbsyqlb0yrry7as29rccsz";
            };
            propagatedBuildInputs = with oself; [eio eio_main caqti];
          };

          wasm = oself.buildDunePackage {
            pname = "wasm";
            inherit (super.wasm) version src;

            postPatch = ''
              touch wasm.opam

              substituteInPlace "interpreter/dune" --replace \
                "(name wasm)" \
                "(name wasm) (public_name wasm)"
            '';
          };

          ligo-simple-utils = oself.buildDunePackage rec {
            pname = "simple-utils";
            inherit (self.ligo) version;
            src = "${self.ligo.src}/vendors/ligo-utils/simple-utils";

            propagatedBuildInputs = with oself; [
              base
              core
              yojson
              ppx_deriving
              ppx_deriving_yojson
              ppx_hash
            ];
          };

          ligo-tezos-utils = oself.buildDunePackage rec {
            pname = "tezos-utils";
            inherit (self.ligo) version;
            src = "${self.ligo.src}/vendors/ligo-utils/tezos-utils";

            propagatedBuildInputs = with oself; [
              tezos-error-monad
              tezos-stdlib-unix
              tezos-micheline
              tezos-base
              data-encoding
              ligo-simple-utils
              base
            ];
          };

          ligo-memory-proto-alpha = oself.buildDunePackage rec {
            pname = "tezos-memory-proto-alpha";
            inherit (self.ligo) version;
            src = "${self.ligo.src}/vendors/ligo-utils/memory-proto-alpha";

            propagatedBuildInputs = with oself; [
              tezos-protocol-environment
              tezos-014-PtKathma.protocol
            ];
          };

          proto-alpha-utils = oself.buildDunePackage rec {
            pname = "proto-alpha-utils";
            inherit (self.ligo) version;
            src = "${self.ligo.src}/vendors/ligo-utils/proto-alpha-utils";

            propagatedBuildInputs = with oself; [
              base
              bigstring
              calendar
              cohttp-lwt-unix
              cstruct
              ezjsonm
              hex
              hidapi
              ipaddr
              macaddr
              irmin
              js_of_ocaml
              lwt
              lwt_log
              mtime
              ocplib-endian
              ocp-ocamlres
              re
              rresult
              stdio
              uri
              uutf
              zarith
              ocplib-json-typed
              ocplib-json-typed-bson
              tezos-crypto
              tezos-error-monad
              tezos-stdlib-unix
              tezos-014-PtKathma.client
              ligo-memory-proto-alpha
              ligo-simple-utils
              ligo-tezos-utils
            ];
          };

          llvm = (super.llvm.override {
            libllvm = llvmPackages_14.libllvm;
          }).overrideAttrs (self: {
              cmakeFlags = self.cmakeFlags ++ [ "-S ../llvm" ];
              postPatch = ''
                substituteInPlace "llvm/bindings/ocaml/llvm/llvm_ocaml.c" --replace \
                  " alloc_custom(" \
                  " caml_alloc_custom("
                substituteInPlace "llvm/bindings/ocaml/llvm/llvm_ocaml.c" --replace \
                  " string_length(" \
                  " caml_string_length("
                substituteInPlace "llvm/bindings/ocaml/llvm/llvm_ocaml.c" --replace \
                  " callback(" \
                  " caml_callback("
                substituteInPlace "llvm/bindings/ocaml/llvm/llvm_ocaml.c" --replace \
                  " failwith(" \
                  " caml_failwith("
                substituteInPlace "llvm/bindings/ocaml/llvm/llvm_ocaml.c" --replace \
                  " remove_global_root(" \
                  " caml_remove_global_root("
                substituteInPlace "llvm/bindings/ocaml/target/target_ocaml.c" --replace \
                  " alloc_custom(" \
                  " caml_alloc_custom("
                substituteInPlace "llvm/bindings/ocaml/target/target_ocaml.c" --replace \
                  " copy_string(" \
                  " caml_copy_string("
              '';
          });
        });
    });
}
