let rec loop n = match n with
| 0 -> ()
| n -> 
  print_endline @@ "working... " ^ string_of_int n;
  Unix.sleep 1;
  loop (n -1)

let () = loop 30

