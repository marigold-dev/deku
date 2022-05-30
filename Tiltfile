# Setup configurations
config.define_string("nodes", False, "specify number of deku nodes to run")
config.define_string("mode", False, "specify what mode to run in, 'docker' (default) or 'local'")
config.define_string("vm", False, "specify the command of the vm")
cfg = config.parse()

no_of_deku_nodes = int(cfg.get('nodes', "3"))
mode = cfg.get('mode', 'docker')

if config.tilt_subcommand == "down":
  local("nix run .#sandbox tear-down")


no_of_deku_nodes = int(cfg.get('nodes', "3"))
path_to_the_vm = cfg.get("vm", 'node ./examples/js-counter/example.js')

def load_config ():
  if mode == "docker" :
    return load_dynamic('./tilt/Tiltfile.docker')
  else:
    return load_dynamic('./tilt/Tiltfile.local')

def get_services(compose):
  return compose.get("services").keys()

symbols = load_config()

add_sandbox = symbols['add_sandbox']
load_deku_services = symbols['load_deku_services']
make_deku_yaml = symbols['make_deku_yaml']

deku_yaml = make_deku_yaml(no_of_deku_nodes)

# Run docker-compose
tezos_yaml = "./docker-compose.yml"
docker_compose([deku_yaml, tezos_yaml])

for index, deku_service in enumerate(get_services(decode_yaml(deku_yaml))):
    dc_resource(deku_service, labels=["deku"], resource_deps=["deku-setup", "deku-vm-%s" % index])

for tezos_service in get_services(read_yaml(tezos_yaml)):
    dc_resource(tezos_service, labels=["tezos"])

def deku_vm_setup(n, vm_args):
  for i in range(n):
    local_resource(
      "deku-vm-%s" % i, 
      serve_cmd="%s data/%s/state_transition" % (vm_args, i),
      allow_parallel=True,
      labels="vms",
      resource_deps=["deku-setup"],
      readiness_probe=probe( # I have to use a readiness probe because when putting the vm in background there is no more reader on the fifo pipe, so when the node try to send the tx to the vm it fails with a Unix.EPIPE error
        initial_delay_secs=1,
        exec=exec_action(['true'])  # After one seconds, the vm is considered running # TODO: find a better probe
      )
    )

deku_vm_setup(no_of_deku_nodes, path_to_the_vm)

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
  