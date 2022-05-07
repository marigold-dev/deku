# Setup configurations
config.define_string("nodes", False, "specify number of deku nodes to run")
config.define_string("mode", False, "specify what mode to run in, 'docker' (default) or 'local'")

if config.tilt_subcommand == "down":
  local("nix run .#sandbox tear-down")

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
tezos_yaml = "./docker-compose.yml"
docker_compose([deku_yml, tezos_yaml])

for deku_service in get_services(decode_yaml(deku_yml)):
    dc_resource(deku_service, labels=["deku"], resource_deps=["deku-setup"])

for tezos_service in get_services(read_yaml(tezos_yaml)):
    dc_resource(tezos_service, labels=["tezos"])

def deku_vm_setup(n, vm_args):
  for i in range(n):
    local_resource(
      "deku-vm-%s" % i,
      "%s ./data/%s/state_transition" % (vm_args, i,),
      resource_deps=["deku-setup"],
      labels=["vms"],
    )


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
