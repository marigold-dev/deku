# Nix parameters function definition
{ pkgs, lib, nix-filter, deku }:

# Nix profiling: deku with profiling: 
# perf record command (load-test)
# mkDerivation{}: function take 

pkgs.stdenv.mkDerivation rec {
  pname = "profiling";
  version = deku.version;

  # Call the profiling script, we can point to that folder
  src = null;

  # Multiplines strings
  buildPhase = ''
  echo "nothing"
  '';

  # Anything needed to build, call the deku that define above
  # https://search.nixos.org/packages: searching for packages
  propagatedBuildInputs = with pkgs;
    [
        deku
        perf-tools
    ]
  
  # meta.mainProgram = 

}