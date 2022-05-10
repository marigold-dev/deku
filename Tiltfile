config.define_string("nodes", False, "specify number of deku nodes to run")
config.define_string("vm", False, "specify the command of the vm")
cfg = config.parse()

no_of_deku_nodes = int(cfg.get('nodes', "3"))
path_to_the_vm = cfg.get("vm", 'node ./examples/js-counter/example.js')

def get_services(compose):
    return compose.get("services").keys()

def make_deku_yaml(n):
  services = []

  for i in range(n):
    deku_node_name = "deku-node-%s" % i
    services.append((deku_node_name, {
    'container_name': deku_node_name,
    'restart': 'always',
    'image': 'ghcr.io/marigold-dev/deku',
    'expose': [ '444%s' % i ],
    'volumes': [ ("./data/%s:/app/data" % i) ],
    'network_mode': 'host' # So that each node can access localhost host
    }))

  services = {k: v for k, v in services}
  return encode_yaml({
    'version': '3.8',
    'services': services
  })

deku_yaml = make_deku_yaml(no_of_deku_nodes)

# Run docker-compose
tezos_yaml = "./docker-compose.yml"
docker_compose([deku_yaml, tezos_yaml])

for deku_service in get_services(decode_yaml(deku_yaml)):
    dc_resource(deku_service, labels=["deku"], resource_deps=["deku-setup"])

for tezos_service in get_services(read_yaml(tezos_yaml)):
    dc_resource(tezos_service, labels=["tezos"])

def deku_vm_setup(n, vm_args):
  for i in range(n):
    local_resource(
      "deku-vm-%s" % i, 
      serve_cmd="%s data/%s/state_transition" % (vm_args, i),
      allow_parallel=True,
      labels="vms",
      resource_deps=["deku-setup", "deku-node-0"],
      readiness_probe=probe( # I have to use a readiness probe because when putting the vm in background there is no more reader on the fifo pipe, so when the node try to send the tx to the vm it fails with a Unix.EPIPE error
        initial_delay_secs=1,
        exec=exec_action(['true'])  # After one seconds, the vm is considered running # TODO: find a better probe
      )
    )

deku_vm_setup(no_of_deku_nodes, path_to_the_vm)

for deku_service in get_services(decode_yaml(deku_yaml)):
    dc_resource(deku_service, labels=["deku"], resource_deps=["deku-setup"])

custom_build(
  'ghcr.io/marigold-dev/deku', # image name, should match with what's in docker-compose
  'nix build .#docker && docker load < ./result',
  ["./src", "./ppx_lambda_vm", "./ppx_let_binding", "./nix"], # folders to watch f or changes
  skips_local_docker=False,
  tag = "latest")


# run setup when we build
local_resource(
  "deku-setup",
  "sleep 10 && ./sandbox.sh setup tilt %s" % no_of_deku_nodes,
  resource_deps=["flextesa"],
  labels=["scripts"],
  )

# bootstrap the deku network, it will be run after deku-setup and the nodes have started
local_resource(
  "deku-net",
  "./sandbox.sh start tilt %s" % no_of_deku_nodes,
  env = {'NODES': str(no_of_deku_nodes)},
  labels=["scripts"],
  resource_deps=["deku-setup"] + ["deku-vm-%s" % i for i in range(0, no_of_deku_nodes)],
  )

# action to manually trigger a teardown, this should almost never be needed
local_resource(
  "deku-tear-down",
  "./sandbox.sh tear-down",
  auto_init=False,
  trigger_mode=TRIGGER_MODE_MANUAL,
  labels=["scripts"],
  )
  