FROM oxheadalpha/flextesa:20220715

# To prevent flextesa from wiping the initial state
# on startup, we've also added the --keep-root option to
# in kathmandubox
COPY ./kathmandubox /usr/bin/kathmandubox

# Tarball of the state of a flexsesa chain with
# the consensus contract already deployed.
# See contract storage in ../networks/flextesa
COPY ./deku_flextesa_state.tar /tmp/deku_flextesa_state.tar
RUN tar -xvf /tmp/deku_flextesa_state.tar -C /tmp
COPY .tezos-client /root
