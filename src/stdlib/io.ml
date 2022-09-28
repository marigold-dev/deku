let file_exists file =
  match Unix.stat file with _stat -> true | exception _ -> false
