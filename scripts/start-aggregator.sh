
dune build || exit 1

export DEKU_SECRET="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq" # TODO: replace this by the deku secret
export DEKU_NODE_URI="127.0.0.1:4440"
export DEKU_PORT="5550"

_build/install/default/bin/deku-aggregator