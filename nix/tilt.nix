{ pkgs }:

pkgs.writeShellApplication {
  name = "run_tilt.sh";
  runtimeInputs = with pkgs; [ kubectl tilt kind docker ];
  text = ''
    KIND_CLUSTER_NAME="deku-dev"
    
    ${builtins.readFile ./scripts/kind-with-registry.sh}

    tilt up --file Tiltfile.k8s
  '';
  checkPhase = "true";
}
