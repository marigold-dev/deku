FROM alpine:latest as certs
RUN apk --update add ca-certificates

FROM esydev/esy:nightly-alpine-latest as builder

RUN apk add libexecinfo-dev

WORKDIR /app

# Add things that doesn't change much, or should bust cache when it does
COPY ./esy.lock package.json ./

RUN esy install
RUN esy build-dependencies --release

# Copy the rest of the files
COPY . .

# TODO: investigate why esy complains that it's not installed if we don't install again
RUN esy install
RUN esy build
RUN esy build_static

# Copy the static binaries to a known location
RUN esy cp "#{self.target_dir / 'default' / 'src' / 'bin' / 'sidecli.exe'}" sidecli.exe && \
    esy cp "#{self.target_dir / 'default' / 'src' / 'bin' / 'deku_node.exe'}" deku_node.exe
RUN strip ./deku_node.exe && strip ./sidecli.exe

FROM scratch as runtime

# Setup OPENSSL so that it finds the certs
ENV OPENSSL_STATIC=1
ENV SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
ENV SSL_CERT_DIR=/etc/ssl/certs
COPY --from=certs /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/

WORKDIR /app

COPY --from=builder /app/deku_node.exe deku_node.exe
COPY --from=builder /app/sidecli.exe sidecli.exe

ENTRYPOINT ["/app/deku_node.exe"]
CMD ["/app/data"]
