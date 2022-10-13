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
  script = pkgs.writeScriptBin "deku-node" ''
    mkdir -p /var/lib/deku
    mkdir -p /run/deku
    mkfifo /run/deku/pipe_read
    mkfifo /run/deku/pipe_write
    echo "hello world"
    ls ${deku}/bin
    sleep 1000000
  '';
in
pkgs.dockerTools.buildImage {
  name = "ghcr.io/marigold-dev/deku";
  tag = "latest";

  fromImage = baseImage;

  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    pathsToLink = [ "/app" "/bin" "/var/lib/deku" ];
    paths = [ script pkgs.bash ];
  };
  config = {
    author = "marigold.dev";
    architecture = "amd64";
    os = "linux";

    Env = [
      "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
      "NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
    ];
    WorkingDir = "/app";
    Entrypoint = "${script}/bin/deku-node";
  };
}
