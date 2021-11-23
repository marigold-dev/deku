FROM esydev/esy:nightly-alpine-latest

RUN apk add libexecinfo-dev
WORKDIR /app
COPY . .
RUN esy
RUN esy release
WORKDIR _release
RUN npm pack
RUN npm i -g ./deku-0.1.0.tgz --prefix /usr/local
ENTRYPOINT ["deku-node"]
CMD ["/app/data"]
