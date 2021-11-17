let () = 
  let argv = Sys.argv in
  let result = Cli.run ~argv () in
  Stdlib.exit result
