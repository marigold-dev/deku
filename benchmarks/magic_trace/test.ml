(*

https://github.com/janestreet/magic-trace/wiki/Supported-platforms,-programming-languages,-and-runtimes

https://github.com/janestreet/magic-trace


  - Using perf with Intel PT (Intel Processor Trace)

  $ perf record -e intel_pt//

  needs to use Ctrl+C to stop after some time
  
  it will generate the perf.data 

  - To trace with `ls` userspace-only:
  $ perf record -e intel_pt//u ls

  - To represent software control flow:
  $ sudo perf record -e intel_pt//u ls
  $ sudo perf script --itrace=ibxwpe

  #####
  Run magic-trace

  Download the magic-trace
  Run: chmod +x magic-trace
  Then: ./magic-trace --help

  Using `magic-trace` is like using `perf`

  Showing PID by : $ ps aux | grep <process-to-find>

  Example:

*)