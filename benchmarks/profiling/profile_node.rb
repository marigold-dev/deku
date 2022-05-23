#
# Profile deku node with perf
# - Tear down the deku node
# - Setup deku environment
# - Start deku clusters and use perf to get the profiling data
#  + -F 99: Sampling CPU stacks at 99 Hertz*
#  + -g: enable call-graph (stack chain/backtrace) recording for both kernel space 
#        and user space
#  + --call-graph dwarf: set up and enable call-graph mode, get backtraces by using dwarf option
#  + -a: samples across all CPUs
# - Sleep for 10 seconds
#
# Note:
# The rate for perf is 99 Hertz, the generated flame graph from a 1000 Hertz profile.
# Choosing 99 Hertz instead of 100 Hertz, is to avoid accidentally sampling in 
# lockstep with some periodic activity, which would produce skewed results.
# You can also increase to higher rates (eg, up to 997 Hertz) for finer resolution.
# Bear in mind that higher frequencies means higher overhead
#
# Note: Setting `perf` to run without super-user permission (allow non-root users)
# sudo sh -c 'echo kernel.perf_event_paranoid=1 > /etc/sysctl.d/local.conf'

profile_node=
command ="cd .. && cd .. && ./sandbox.sh tear-down \
          && ./sandbox.sh setup \
          && perf record -F 99 -a -g --call-graph dwarf ./sandbox.sh start
          && sleep 10"
system (command)