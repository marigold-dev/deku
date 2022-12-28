#!/usr/bin/env bash

cargo run --release  -- ./networks/betanets/dpp/data.gb -t 2 -a 2>&1 | tee "logs_gb"
