# Run docker-compose
docker_compose(["./docker-compose.yml", "./docker-compose.override.yml"])

dc_resource("db", labels=["database"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)
dc_resource("elastic", labels=["database"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)

dc_resource("flextesa", labels=["tezos"])
dc_resource("gui", labels=["tezos"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)
dc_resource("api", labels=["tezos"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)

dc_resource("metrics", labels=["infra"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)
dc_resource("indexer", labels=["infra"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)
dc_resource("prometheus", labels=["infra"], trigger_mode=TRIGGER_MODE_MANUAL, auto_init=False)

# deku nodes should not start before we have run the setup since they depend on state generated there
dc_resource("deku-node-0", labels=["deku"], resource_deps=["deku-setup"])
dc_resource("deku-node-1", labels=["deku"], resource_deps=["deku-setup", "deku-node-0"])
dc_resource("deku-node-2", labels=["deku"], resource_deps=["deku-setup", "deku-node-1"])

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
