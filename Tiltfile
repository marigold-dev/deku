# Setup configurations
config.define_string("nodes", False, "specify number of deku nodes to run")
config.define_string("mode", False, "specify what mode to run in, 'docker' (default) or 'local'")

cfg = config.parse()

no_of_deku_nodes = int(cfg.get('nodes', "3"))
mode = cfg.get('mode', 'docker')

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

dc_resource("db", labels=["database"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)
dc_resource("elastic", labels=["database"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)

dc_resource("flextesa", labels=["tezos"])
dc_resource("gui", labels=["tezos"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)
dc_resource("api", labels=["tezos"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)

dc_resource("metrics", labels=["infra"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)
dc_resource("indexer", labels=["infra"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)
dc_resource("prometheus", labels=["infra"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)

load_deku_services(deku_yaml)

add_sandbox(no_of_deku_nodes)

# action to manually trigger a teardown, this should almost never be needed
local_resource(
  "deku-tear-down",
  "./sandbox.sh tear-down",
  auto_init=False,
  trigger_mode=TRIGGER_MODE_MANUAL,
  labels=["scripts"],
  )
