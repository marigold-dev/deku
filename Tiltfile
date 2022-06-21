if config.tilt_subcommand == "down":
  local("nix run .#sandbox tear-down")

variables = load_dynamic('./tilt/Tiltfile.variables')

mode = variables['mode']
no_of_deku_nodes = variables['no_of_deku_nodes']
deku_nodes = variables['deku_nodes']
is_not_ci = variables['is_not_ci']

def load_config ():
  if mode == "docker" :
    return load_dynamic('./tilt/Tiltfile.docker')
  else:
    return load_dynamic('./tilt/Tiltfile.local')

symbols = load_config()

add_sandbox = symbols['add_sandbox']
load_deku_services = symbols['load_deku_services']
make_deku_yaml = symbols['make_deku_yaml']

deku_yaml = make_deku_yaml(deku_nodes)

# Run docker-compose
docker_compose(["./docker-compose.yml", deku_yaml])

dc_resource("db", labels=["tezos"], auto_init=is_not_ci)
dc_resource("elastic", labels=["tezos"], auto_init=is_not_ci)
dc_resource("flextesa", labels=["tezos"])
dc_resource("gui", labels=["tezos"], auto_init=is_not_ci)
dc_resource("api", labels=["tezos"], auto_init=is_not_ci)
dc_resource("metrics", labels=["tezos"], auto_init=is_not_ci)
dc_resource("indexer", labels=["tezos"], auto_init=is_not_ci)

dc_resource("prometheus", labels=["infra"])

add_sandbox(no_of_deku_nodes)
load_deku_services(deku_nodes)

# action to manually trigger a teardown, this should almost never be needed
local_resource(
  "deku-tear-down",
  "nix run .#sandbox tear-down",
  auto_init=False,
  trigger_mode=TRIGGER_MODE_MANUAL,
  labels=["scripts"],
  )
