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
    'expose': [ 4040 ],
    'volumes': [ ("./data/%s:/app/data" % i) ],
    }))

  services = {k: v for k, v in services}
  return encode_yaml({
    'version': '3.8',
    'services': services
  })

deku_yaml = make_deku_yaml(3)
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

for deku_service in get_services(decode_yaml(deku_yaml)):
    dc_resource(deku_service, labels=["deku"], resource_deps=["deku-setup"])

custom_build(
  'ghcr.io/marigold-dev/deku', # image name, should match with what's in docker-compose
  'nix build .#docker && docker load < ./result',
  ["./src", "./ppx_lambda_vm", "./ppx_let_binding", "./nix"], # folders to watch f or changes
  skips_local_docker=False,
  tag = "latest")

# since everyone won't have dune available in their environment these are removed for now
# run dune build and tests on changes
# local_resource(
#   "build",
#   "dune build @install --force",
#   deps=["src", "nix", "ppx_lambda_vm", "ppx_let_binding"],
#   labels=["deku"],
#   )
# local_resource(
#   "tests",
#   "dune runtest --force",
#   deps=["src", "tests"],
#   labels=["deku"],
#   )

# run setup when we build
local_resource(
  "deku-setup",
  "sleep 10 && ./sandbox.sh setup docker",
  resource_deps=["flextesa"],
  labels=["scripts"],
  )

# bootstrap the deku network, it will be run after deku-setup and the nodes have started
local_resource(
  "deku-net",
  "./sandbox.sh start docker",
  resource_deps=["deku-setup", "deku-node-0", "deku-node-1", "deku-node-2"],
  labels=["scripts"],
  auto_init=True, # trigger once at start
  trigger_mode=TRIGGER_MODE_MANUAL,
  )

# action to manually trigger a teardown, this should almost never be needed
local_resource(
  "deku-tear-down",
  "./sandbox.sh tear-down",
  auto_init=False,
  trigger_mode=TRIGGER_MODE_MANUAL,
  labels=["scripts"],
  )
