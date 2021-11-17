module A = struct
  open Rope

  let _ = d
  let _ = ~%d
  let _ = (~%d) #% 42
  let _ = (~%d%d)
  let _ = (~%d%d) #% 42 43
  let _ = (~%d%s) #% 42 "foo"
  let _ = (~%(S"foo")%s) #% ""
  let _ = (~%d%S"tralala"%d%s) #% 42 43 "foo"
end

module B = struct
  open Rope_top_level_open

  (* type foo = S | NotCaptured *)
  (* let d = NotCaptured *)
  (* let s = NotCaptured *)

  let _ = Rope.(~%d) #% 42
  let _ = Rope.(~%d%d)
  let _ = Rope.(~%d%d) #% 42 43
  let _ = Rope.(~%d%s) #% 42 "foo"
  let _ = Rope.(~%(S"foo")%s) #% ""
  let _ = Rope.(~%d%S"tralala"%d%s) #% 42 43 "foo"
end
