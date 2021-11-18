FROM alekzonder/puppeteer:latest as puppeteer

WORKDIR /app

COPY package.json package.json
COPY package-lock.json package-lock.json
COPY jest-puppeteer.config.js jest-puppeteer.config.js
COPY test test

RUN npm ci

ENTRYPOINT [ "npm", "run", "test" ]
