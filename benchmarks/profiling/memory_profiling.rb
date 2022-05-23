#
# Memory profiling the C heap using `valgrind` and `massif-visualizer`

memory_profile=
command="cd .. && cd .. && ./sandbox.sh tear-down \
         && ./sandbox.sh setup \
         && valgrind --tool=massif ./sandbox.sh start"
system(command)