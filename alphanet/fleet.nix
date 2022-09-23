{ nixpkgs, deploy-rs, deku-node, rev }:
let
  network_config_to_nixos_config = { bootstrapper_key, tezos_consensus_address, tezos_discovery_address, nodes }:
    let
      validators = builtins.concatStringsSep "," (builtins.map ({ validator, ... }: validator) nodes);
      validator_uris = builtins.concatStringsSep "," (builtins.map ({ validator_uri, ... }: "http://${validator_uri}:4440") nodes);
    in
    builtins.foldl'
      (nodes: { name, secret, validator, validator_uri, tezos_secret, tezos_rpc_node }:
        nodes // {
          ${name} =
            {
              hostname = validator_uri;
              profiles.system = {
                user = "root";
                path = deploy-rs.lib.x86_64-linux.activate.nixos
                  (
                    let system = "x86_64-linux"; in
                    nixpkgs.lib.nixosSystem {
                      inherit system;
                      modules = [
                        (import ./config/modules/node-exporter.nix)
                        (import ./config/modules/promtail.nix)
                        deku-node
                        ({ pkgs, ... }:
                          {
                            networking.hostName = name;
                            system.stateVersion = "22.11";
                            services.openssh = {
                              enable = true;
                              passwordAuthentication = false;
                            };
                            users.users.root.openssh.authorizedKeys.keys = import ./config/authorized_keys.nix;
                            services.deku-node = {
                              enable = true;
                              openFirewall = true;
                              environment = {
                                DEKU_SECRET = secret;
                                DEKU_BOOTSTRAP_KEY = bootstrapper_key;
                                DEKU_VALIDATORS = validators;
                                DEKU_VALIDATOR_URIS = validator_uris;
                                DEKU_TEZOS_SECRET = tezos_secret;
                                DEKU_TEZOS_RPC_NODE = tezos_rpc_node;
                                DEKU_TEZOS_CONSENSUS_ADDRESS = tezos_consensus_address;
                                DEKU_TEZOS_DISCOVERY_ADDRESS = tezos_discovery_address;
                                DEKU_DATABASE_URI = "sqlite3:///var/lib/deku/db.sqlite";
                                DEKU_DATA_FOLDER = "/var/lib/deku";
                                DEKU_DEFAULT_BLOCK_SIZE = "1";
                                DEKU_API_ENABLED = "true";
                              };
                            };

                            # Stuff to help with debugging, etc.
                            environment.systemPackages = with pkgs; [
                              vim
                              bc
                              sqlite
                              termdbms
                              curl
                              htop
                            ];
                            environment.etc."deku/revision".text = "${rev}";
                            programs.tmux.enable = true;
                            programs.zsh.enable = true;
                          })
                        # EC2 specific config
                        ({ modulesPath, ... }: {
                          imports = [ "${modulesPath}/virtualisation/google-compute-image.nix" ];
                        })
                      ];
                    }
                  );
              };
            };
        })
      { }
      nodes;
  decookies_network = network_config_to_nixos_config (import ./config/decookies_network.nix);
  # TODO: depoy deku-c
  deku_c_network = { }; # network_config_to_nixos_config (import ./configs/deku_c_network.nix);
in
decookies_network // deku_c_network
