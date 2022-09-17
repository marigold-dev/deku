{ lib, stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "ligo";
  version = "0.49.0";

  executable = fetchurl {
    name = "ligo";
    url = "https://gitlab.com/ligolang/ligo/-/jobs/2896100663/artifacts/raw/ligo";
    sha256 = "sha256-gNNjePpp2Ws3KQ9zmO+/j5dYkpmQi0msrQlogzye2fY=";
    executable = true;
  };

  phases = [ "installPhase" ]; # Removes all phases except installPhase

  installPhase = "
    mkdir -p $out/bin
    cp ${executable} $out/bin/ligo
  ";
}
