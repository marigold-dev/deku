
FROM esydev/esy:nightly-alpine-latest as builder

RUN apk add libexecinfo-dev go ca-certificates gmp-dev libev nodejs go

WORKDIR /app

# Add things that doesn't change much, or should bust cache when it does
COPY ./esy.lock esy.json ./

RUN esy install
RUN esy build-dependencies --release

# Copy the rest of the files
COPY . .

# TODO: investigate why esy complains that it's not installed if we don't install again
RUN esy install
RUN esy build

# Build the Go components
RUN cd ./state_transition && go build

# Copy the static binaries to a known location
RUN esy cp "#{self.target_dir / 'default' / 'src' / 'bin' / 'deku_cli.exe'}" deku-cli && \
    esy cp "#{self.target_dir / 'default' / 'src' / 'bin' / 'deku_node.exe'}" deku-node

CMD /app/deku-node /app/data /app/state_transition/state_transition
