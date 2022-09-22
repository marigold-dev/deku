{ deku-packages }: { config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.deku-node;
  listToString = lib.strings.concatStringsSep ",";
in
{
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
          after = [ "network.target" ];
          wantedBy = [ "multi-user.target" ];
          path = [ pkgs.nodejs-16_x ];
          environment = cfg.environment;
          serviceConfig = {
            Type = "simple";
            ExecStart = "${deku-packages.${config.nixpkgs.system}.default}/bin/deku-node --named-pipe-path /var/run/deku/pipe";
            Restart = "on-failure";
            StateDirectory = "deku";
            RuntimeDirectory = "deku";
            MemoryMax = "55G";

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

        deku-cookie-vm = {
          description = "Deku cookie VM";
          after = [ "network.target" ];
          wantedBy = [ "multi-user.target" ];
          before = [ "deku-node.service" ];
          path = [ pkgs.nodejs-16_x ];
          environment = cfg.environment;
          serviceConfig = {
            Type = "simple";
            ExecStart = "${pkgs.nodejs-16_x}/bin/node ${deku-packages.${config.nixpkgs.system}.cookie-game}/lib/node_modules/cookie-game/lib/src/index.js /var/run/deku/pipe";
            Restart = "on-failure";
            StateDirectory = "deku";
            RuntimeDirectory = "deku";

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
      };

      sockets = {
        deku-vm = {
          description = "Sockets to communicate between Deku and VM";
          unitConfig = { RequiresMountsFor = "/tmp/deku"; };
          socketConfig = { ListenFIFO = [ "/var/run/deku/pipe_read" "/var/run/deku/pipe_write" ]; };
          before = [ "deku-node.service" "deku-cookie-vm.service" ];
        };
      };
    };

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [ cfg.port ];
    };
  };
}
