/** [make_pipe_pair path] Makes two unix named pipes (FIFO pipes).
    The path of the first is [path] suffixed with "_read", the path
    of the second is [path] suffixed with "_write". */
let make_pipe_pair: string => unit;

/** [get_pipe_pair_channels path] returns a pair containing input and output channels
    for the pipes created with [make_pipe_pair path]. */
let get_pipe_pair_channels:
  string => (Lwt_io.channel(Lwt_io.input), Lwt_io.channel(Lwt_io.output));
