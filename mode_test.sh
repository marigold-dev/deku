#!/usr/bin/env bash


TIME="0.25"

for n in {0..15}; do
  rm ./chain/data/0/database.db ./chain/data/1/database.db ./chain/data/2/database.db ./chain/data/3/database.db 
  echo "sleep: $TIME" >> ./model_data/produce_data
  ./start.sh 
  ./scripts/count_tps.sh >> ./model_data/produce_data
done
