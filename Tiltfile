config.define_string("nodes", False, "specify number of deku nodes to run")
cfg = config.parse()
no_of_deku_nodes = int(cfg.get('nodes', "3"))

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
    'expose': [ 4440 ],
    'volumes': [ ("./data/%s:/app/data" % i) ],
    }))

  services = {k: v for k, v in services}
  return encode_yaml({
    'version': '3.8',
    'services': services
  })

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
  "sleep 10 && ./sandbox.sh setup docker %s" % no_of_deku_nodes,
  resource_deps=["flextesa"],
  labels=["scripts"],
  )

# bootstrap the deku network, it will be run after deku-setup and the nodes have started
local_resource(
  "deku-net",
  "./sandbox.sh start docker %s" % no_of_deku_nodes,
  resource_deps=["deku-setup", "deku-node-0", "deku-node-1", "deku-node-2"],
  "nix run .#sandbox -- start docker use_nix",
  resource_deps=(["deku-setup"] + get_services(decode_yaml(deku_yml))),
  env = {'NODES': str(DEKU_NODES)},
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
