{ deku }: { config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.deku-node;
  listToString = lib.strings.concatStringsSep ",";
in
{
  options.services.deku-node = {
    enable = mkEnableOption "deku node";
    # FIXME: we should probably create real config options here but for now
    # I don't want to maintain the same options in many places.
    environment = mkOption {
      type = types.attrsOf types.str;
      description = lib.mdDoc "Environment variables passed to the Deku node";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.deku-node = {
      description = "Deku node";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.nodejs-16_x ];
      environment = cfg.environment;
      serviceConfig = {
        Type = "simple";
        ExecStart = "${deku}/bin/deku-node";
        Restart = "on-failure";
        StateDirectory = "deku";

        # Basic Hardening
        NoNewPrivileges = "yes";
        PrivateTmp = "yes";
        PrivateDevices = "yes";
        DevicePolicy = "closed";
        DynamicUser = "true";
        ProtectSystem = "strict";
        ProtectHome = "read-only";
        ProtectControlGroups = "yes";
        ProtectKernelModules = "yes";
        ProtectKernelTunables = "yes";
        RestrictAddressFamilies = "AF_UNIX AF_INET AF_INET6 AF_NETLINK";
        RestrictNamespaces = "yes";
        RestrictRealtime = "yes";
        RestrictSUIDSGID = "yes";
        MemoryDenyWriteExecute = "no";
        LockPersonality = "yes";
      };
    };

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [ cfg.port ];
    };
  };
}
