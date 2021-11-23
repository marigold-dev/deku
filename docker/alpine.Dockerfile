FROM esydev/esy:nightly-alpine-latest

RUN apk add libexecinfo-dev
WORKDIR /app
COPY . .
RUN esy
RUN esy release
WORKDIR _release
RUN npm pack
RUN npm i -g ./sidechain-0.0.0.tgz --prefix /usr/local
ENTRYPOINT ["deku-node"]
CMD ["/app/data"]
