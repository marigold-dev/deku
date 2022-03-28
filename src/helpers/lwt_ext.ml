module Let_syntax = struct
  let await = Lwt.return
  [%%let "let.await", Lwt.bind]

  let ( let* ) = Lwt.bind
end
