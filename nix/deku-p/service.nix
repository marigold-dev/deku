{deku-packages}: {
  config,
  pkgs,
  lib,
  ...
}:
with lib; let
  cfg = config.services.deku-node;
  listToString = lib.strings.concatStringsSep ",";
in {
  options.services.deku-node = {
    enable = mkEnableOption "deku node";
    port = mkOption {
      type = types.int;
      default = 4440;
      description = lib.mDoc "The port to listen to";
    };
    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = lib.mdDoc "Open ports in the firewall for the deku node.";
    };
    # FIXME: we should probably create real config options here but for now
    # I don't want to maintain the same options in many places.
    environment = mkOption {
      type = types.attrsOf types.str;
      description = lib.mdDoc "Environment variables passed to the Deku node";
    };
  };

  config = mkIf cfg.enable {
    systemd = {
      services = {
        deku-node = {
          description = "Deku node";
          after = ["network.target"];
          wantedBy = ["multi-user.target"];
          path = [pkgs.nodejs-16_x];
          environment = cfg.environment;
          serviceConfig = {
            Type = "simple";
            ExecStart = "${deku-packages.${config.nixpkgs.system}.default}/bin/deku-node";
            Restart = "on-failure";
            StateDirectory = "deku";
            RuntimeDirectory = "deku";
            RuntimeDirectoryPreserve = "yes";
            MemoryMax = "155G";

            # Basic Hardening
            # NoNewPrivileges = "yes";
            # PrivateTmp = "yes";
            # PrivateDevices = "yes";
            # DevicePolicy = "closed";
            # DynamicUser = "true";
            # ProtectSystem = "strict";
            # ProtectHome = "read-only";
            # ProtectControlGroups = "yes";
            # ProtectKernelModules = "yes";
            # ProtectKernelTunables = "yes";
            # RestrictAddressFamilies = "AF_UNIX AF_INET AF_INET6 AF_NETLINK";
            # RestrictNamespaces = "yes";
            # RestrictRealtime = "yes";
            # RestrictSUIDSGID = "yes";
            # MemoryDenyWriteExecute = "no";
            # LockPersonality = "yes";
          };
        };
        deku-api = {
          description = "Deku api";
          after = ["network.target"];
          wantedBy = ["multi-user.target"];
          path = [pkgs.nodejs-16_x];
          environment = cfg.environment;
          serviceConfig = {
            Type = "simple";
            ExecStart = "${deku-packages.${config.nixpkgs.system}.default}/bin/deku-api";
            Restart = "on-failure";
            StateDirectory = "deku_api";
            RuntimeDirectory = "deku_api";
            RuntimeDirectoryPreserve = "yes";
          };
        };
      };
        };
    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [
        cfg.port
        # FIXME: stop hard coding this
        8080
      ];
    };
  };
}
