# Profiling Deku-node

## Profiling Deku-node using Linux `perf`

```
ruby profile_node.rb
```

Profile deku node with perf:
- Tear down the deku node
- Setup deku environments
- Start deku clusters and use perf to get the profiling data
  + `-F 99`: Sampling CPU stacks at 99 Hertz*
  + `--callgraph dwarf`: last branch record
  + `-a`: samples across all CPUs
 - Sleep for 10 seconds

**Note:**
The rate for perf is 99 Hertz, the generated flame graph from a 1000 Hertz profile.
Choosing 99 Hertz instead of 100 Hertz, is to avoid accidentally sampling in 
lockstep with some periodic activity, which would produce skewed results.
You can also increase to higher rates (eg, up to 997 Hertz) for finer resolution.
Bear in mind that higher frequencies means higher overhead

**Note:** Setting `perf` to run without super-user permission (allow non-root users)

```
sudo sh -c 'echo -1 >/proc/sys/kernel/perf_event_paranoid'
 sudo sysctl -w kernel.perf_event_paranoid=-1
sudo sh -c 'echo kernel.perf_event_paranoid=-1 > /etc/sysctl.d/local.conf'
```

`perf_event_paranoid` setting is 1:
- `-1`: Allow use of (almost) all events by all users
      Ignore mlock limit after perf_event_mlock_kb without CAP_IPC_LOCK
- `>= 0`: Disallow raw and ftrace function tracepoint - access
- `>= 1`: Disallow CPU event access
- `>= 2`: Disallow kernel profiling

To make the adjusted `perf_event_paranoid` setting permanent preserve it
in `/etc/sysctl.conf` (e.g. `kernel.perf_event_paranoid = <setting>`)

## Flamegraph

Read `perf.data` using FlameGraph. FlameGraph needs to be downloaded at:
 `git clone https://github.com/brendangregg/FlameGraph `

Run this script only when `perf record` is **properly terminated** (a.k.a: `ruby profile_node.rb`).

```
ruby flamegraph.rb
```

Wait for awhile to let the graph generated. The result in picture as `perf-flamegraph.svg`. You can use internet browser to see the file `perf-flamegraph.svg`:

 `firefox perf-flamegraph.svg` or chrome, etc.

 
### Understand Flamegraph
It shows: 
- x-axis: show the stack profile (the sample) population, sorted alphabetically
- y-axis: stack depth, counting from zero at the bottom.
- Each function (stack frame) is drawn as a rectangle, with the width relative to the number of samples. The wider a frame is, the more often it was present in the stacks.
- The top edge shows what is on-CPU.
- The beneath it is its ancestry.
- The colors are usually not significant, picked randomly to differentiate frames.