{ deku }: { pkgs, ... }:
{
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    binaryCaches = [
      "https://nix-community.cachix.org"
      "https://anmonteiro.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "anmonteiro.cachix.org-1:KF3QRoMrdmPVIol+I2FGDcv7M7yUajp4F2lt0567VA4="
    ];
  };
  environment.systemPackages = with pkgs; [ git vim curl sqlite termdbms bc deku ];
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  programs.zsh.enable = true;
  programs.tmux.enable = true;
  virtualisation.docker.enable = true;
}
