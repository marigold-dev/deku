# Setup configurations
config.define_string("nodes", False, "specify number of deku nodes to run")
config.define_string("mode", False, "specify what mode to run in, 'local' (default) or 'docker'")

if config.tilt_subcommand == "down":
  local("nix run .#sandbox tear-down")

cfg = config.parse()

no_of_deku_nodes = int(cfg.get('nodes', "3"))
mode = cfg.get('mode', 'local')

# Initialize list of deku nodes
deku_nodes = [] 
for i in range(no_of_deku_nodes):
  deku_node_name = "deku-node-%s" % i
  deku_nodes.append(deku_node_name)

def load_config ():
  if mode == "docker" :
    return load_dynamic('./tilt/Tiltfile.docker')
  else:
    return load_dynamic('./tilt/Tiltfile.local')

symbols = load_config()

load_deku_services = symbols['load_deku_services']
make_deku_yaml = symbols['make_deku_yaml']

deku_yaml = make_deku_yaml(deku_nodes)

# Run docker-compose
docker_compose(["./docker-compose.yml", deku_yaml])

dc_resource("db", labels=["database"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)
dc_resource("elastic", labels=["database"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)

dc_resource("flextesa", labels=["tezos"])
dc_resource("gui", labels=["tezos"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)
dc_resource("api", labels=["tezos"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)

dc_resource("metrics", labels=["infra"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)
dc_resource("indexer", labels=["infra"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)
dc_resource("prometheus", labels=["infra"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=True)

load_deku_services(deku_nodes)

local_resource(
  "deku-setup",
  "./tilt/create-deku-environment.sh %s %s" % (no_of_deku_nodes, mode),
  resource_deps=["prepare-tezos", "deku-tear-down"],
  labels=["scripts"],
  allow_parallel=True,
)

local_resource(
  "prepare-tezos",
  "./tilt/prepare-tezos-node.sh",
  resource_deps=["flextesa"],
  auto_init=True,
  trigger_mode=TRIGGER_MODE_MANUAL,
  labels=["scripts"],
  allow_parallel=True,
)

# action to manually trigger a teardown, this should almost never be needed
local_resource(
  "deku-tear-down",
  "nix run .#sandbox tear-down",
  auto_init=True,
  trigger_mode=TRIGGER_MODE_MANUAL,
  labels=["scripts"],
  allow_parallel=True,
)

local_resource(
  "restart network",
  "./tilt/restart-network.sh %s %s" % (no_of_deku_nodes, mode),
  auto_init=False,
  trigger_mode=TRIGGER_MODE_MANUAL,
  labels=["scripts"],
  allow_parallel=True,
)

# CI

if config.tilt_subcommand == "ci":
  # bootstrap the deku network and run some tests
  local_resource(
    "smoke-test",
    "nix run .#sandbox smoke-test %s %s" % (mode, no_of_deku_nodes),
    resource_deps=["deku-setup", "deku-net"],
    labels=["scripts"],
    auto_init=True, # trigger once at start
    trigger_mode=TRIGGER_MODE_MANUAL,
    allow_parallel=True,
  )

  local_resource(
    "deposit-withdraw-test",
    "nix run .#sandbox deposit-withdraw-test %s %s" % (mode, no_of_deku_nodes),
    resource_deps=["smoke-test", "deku-net"],
    labels=["scripts"],
    auto_init=True, # trigger once at start
    trigger_mode=TRIGGER_MODE_MANUAL,
    allow_parallel=True,
  )