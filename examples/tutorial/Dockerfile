FROM node
RUN mkdir /app
WORKDIR /app
COPY package.json /app
RUN npm install
COPY ./vm.js .
CMD node ./vm.js
