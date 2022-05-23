#
# Read perf.data using FlameGraph
# Run this script only when `perf record` is properly terminated (a.k.a: ruby profile_node.rb)
# - Copy perf.data generated in Deku to FlameGraph folder* 
# - Go inside FlameGraph folder and use `perf script` to read the information inside
# perf.data
# - Use flamegraph commands and save the result in picture as `perf-flamegraph.svg`
# - Wait for awhile to let the graph generated, you can use internet browser to see the file
# `perf-flamegraph.svg`:
#  + firefox perf-flamegraph.svg # or chrome, etc.
#
# Note:
# FlameGraph need to be downloaded at:
# git clone https://github.com/brendangregg/FlameGraph 
# 
# Understand Flamegraph
# It shows: 
# - x-axis: show the stack profile (the sample) population, sorted alphabetically
# - y-axis: stack depth, counting from zero at the bottom.
# - Each function (stack frame) is drawn as a rectangle, with the width relative to the
# number of samples. The wider a frame is, the more often it was present in the stacks.
# - The top edge shows what is on-CPU
# - The beneath it is its ancestry
# - The colors are usually not significant, picked randomly to differentiate frames.

flamegraph=
command ="cp ~/deku/perf.data ~/FlameGraph/perf.data \
          && cd ~/FlameGraph && perf script \
          | ./stackcollapse-perf.pl \
          | ./flamegraph.pl > perf-flamegraph.svg"
system(command)
