# Setup configurations
config.define_string("nodes", False, "specify number of deku nodes to run")
config.define_string("mode", False, "specify what mode to run in, 'local' (default) or 'docker'")
config.define_bool("all-services", False, "flag for running all services")

if config.tilt_subcommand == "down":
  local("nix run .#sandbox tear-down")

cfg = config.parse()

no_of_deku_nodes = int(cfg.get('nodes', "3"))
mode = cfg.get('mode', 'local')
all_services = cfg.get('all-services', False)

def load_config ():
  if mode == "docker" :
    return load_dynamic('./tilt/Tiltfile.docker')
  else:
    return load_dynamic('./tilt/Tiltfile.local')

symbols = load_config()

add_sandbox = symbols['add_sandbox']
load_deku_services = symbols['load_deku_services']
make_deku_yaml = symbols['make_deku_yaml']

deku_yaml = make_deku_yaml(no_of_deku_nodes)

# Run docker-compose
docker_compose(["./docker-compose.yml", deku_yaml])

dc_resource("db", labels=["tezos"], auto_init=all_services)
dc_resource("elastic", labels=["tezos"], auto_init=all_services)
dc_resource("flextesa", labels=["tezos"])
dc_resource("gui", labels=["tezos"], auto_init=all_services)
dc_resource("api", labels=["tezos"], auto_init=all_services)
dc_resource("metrics", labels=["tezos"], auto_init=all_services)
dc_resource("indexer", labels=["tezos"], auto_init=all_services)

dc_resource("prometheus", labels=["infra"])

load_deku_services(deku_yaml)

add_sandbox(no_of_deku_nodes)

# action to manually trigger a teardown, this should almost never be needed
local_resource(
  "deku-tear-down",
  "nix run .#sandbox tear-down",
  auto_init=False,
  trigger_mode=TRIGGER_MODE_MANUAL,
  labels=["scripts"],
  )
