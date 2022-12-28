#!/usr/bin/env bash
yarn build:client
yarn --cwd ./examples/deku-plays-pokemon-bot start | tee "logs_bot" &
