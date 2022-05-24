#! /usr/bin/env bash
set -e 

cd .. && cd .. \
      && ./sandbox.sh tear-down \
      && ./sandbox.sh setup \
      && perf record -F 99 -a -g --call-graph dwarf ./sandbox.sh start \
      && sleep 10