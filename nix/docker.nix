{ pkgs, deku }:

let
  baseImage = pkgs.dockerTools.pullImage {
    imageName = "node";
    imageDigest =
      "sha256:bdda9a45df06759f459ec0b4b36646300d9eefa14a0c69a582ab595f5110265c";
    sha256 = "sha256-6glJDZstovsdc6ED3m0CsKaOippKSR0SmSTRTP0f+5Y=";
    finalImageTag = "lts-slim";
    finalImageName = "node";
  };

in pkgs.dockerTools.buildImage {
  name = "ghcr.io/marigold-dev/deku";
  tag = "latest";

  fromImage = baseImage;

  contents = [ deku ];

  config = {
    author = "marigold.dev";
    architecture = "amd64";
    os = "linux";

    Env = [
      "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
      "NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
    ];
    WorkingDir = "/app";
    Entrypoint = [ "${deku}/bin/deku-node" ];
    Cmd = [ "/app/data" ];
  };
}
