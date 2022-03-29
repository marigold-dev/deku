FROM esydev/esy:nightly-alpine-latest as builder

RUN apk add libexecinfo-dev && apk --update add ca-certificates

WORKDIR /app

# Add things that doesn't change much, or should bust cache when it does
COPY ./esy.lock ./esy.lock
COPY ./esy.json ./esy.json

RUN esy install
RUN esy build-dependencies --release

# Copy the rest of the files
COPY . .

RUN esy build --release

ENTRYPOINT [ "esy", "x", "deku-node" ]
CMD [ "/app/data" ]
