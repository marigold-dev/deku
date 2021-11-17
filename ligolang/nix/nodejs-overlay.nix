self: super: {
  # Note: this overlay doesn't apply to nix-npm-buildpackage
  nodejs = super.nodejs-12_x;
  nodePackages = super.nodePackages_12_x;
  nodejs-slim = super.nodejs-slim-12_x;
}
