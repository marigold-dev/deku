{deku-packages}: {
  config,
  pkgs,
  lib,
  ...
}:
with lib; let
  cfg = config.services.deku-node;
  listToString = lib.strings.concatStringsSep ",";
  cookieVM = "${pkgs.nodejs-16_x}/bin/node ${deku-packages.${config.nixpkgs.system}.decookies-vm}/lib/node_modules/decookies-vm/lib/src/index.js";
  wasmVM = "${deku-packages."${config.nixpkgs.system}".vm_library}/bin/vm_library";
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

    vmType = mkOption {
      type = types.str;
      description = lib.mdDoc "The Deku VM that will be used";
      default = "wasm";
    };
  };

  config = mkIf cfg.enable {
    systemd = {
      services = {
        deku-node = {
          description = "Deku node";
          after = ["network.target"];
          requires = ["deku-vm.service"];
          wantedBy = ["multi-user.target"];
          path = [pkgs.nodejs-16_x];
          environment = cfg.environment;
          serviceConfig = {
            Type = "simple";
            ExecStart = "${deku-packages.${config.nixpkgs.system}.default}/bin/deku-node --named-pipe-path /run/deku/pipe";
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

        deku-vm = {
          description = "Deku VM";
          after = ["network.target"];
          wantedBy = ["multi-user.target"];
          before = ["deku-node.service"];
          path = [pkgs.nodejs-16_x];
          environment = cfg.environment;
          serviceConfig = {
            Type = "simple";
            ExecStart = (
              let
                command =
                  if cfg.vmType == "wasm"
                  then wasmVM
                  else cookieVM;
              in "${command} /run/deku/pipe"
            );
            Restart = "on-failure";
            StateDirectory = "deku";
            RuntimeDirectory = "deku";
            RuntimeDirectoryPreserve = "yes";

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
          requires = ["deku-api-vm.service"];
          wantedBy = ["multi-user.target" "deku-node.service"];
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
        deku-api-vm = {
          description = "Deku API VM";
          after = ["network.target"];
          wantedBy = ["multi-user.target"];
          before = ["deku-api.service"];
          path = [pkgs.nodejs-16_x];
          environment = cfg.environment;
          serviceConfig = {
            Type = "simple";
            ExecStart = (
              let
                command =
                  if cfg.vmType == "wasm"
                  then wasmVM
                  else cookieVM;
              in "${command} /run/deku/api_vm_pipe"
            );
            Restart = "on-failure";
            StateDirectory = "deku_api";
            RuntimeDirectory = "deku_api";
            RuntimeDirectoryPreserve = "yes";
          };
        };
      };
      sockets = {
        deku-vm = {
          description = "Sockets to communicate between Deku and VM";
          unitConfig = {RequiresMountsFor = "/run/deku";};
          socketConfig = {ListenFIFO = ["/run/deku/pipe_read" "/run/deku/pipe_write"];};
          before = ["deku-node.service" "deku-vm.service"];
        };
        deku-api-vm = {
          description = "Sockets to communicate between Deku API and VM";
          unitConfig = {RequiresMountsFor = "/run/deku";};
          socketConfig = {ListenFIFO = ["/run/deku/api_vm_pipe_read" "/run/deku/api_vm_pipe_write"];};
          before = ["deku-api.service" "deku-api-vm.service"];
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
