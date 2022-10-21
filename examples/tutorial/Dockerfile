# We'll use the official nodejs
# image as our base, but you should be able
# to use any image.
FROM node

# Copy the deku-node and it's required system libraries
# into your image.
COPY --from=ghcr.io/marigold-dev/deku:latest /nix /nix/
COPY --from=ghcr.io/marigold-dev/deku:latest /bin/deku-node /bin

WORKDIR /app

# Build your Deku VM
COPY package.json /app
RUN npm install
COPY ./vm.js .
COPY ./start.sh .
CMD /app/start.sh
