// Test the pattern matching functionality of PascaLIGO

function match_option (const o : option (int)) : int is
  block {
    var result : int := 23;
    case o of
      None -> skip
    | Some (s) -> block {
        s := 3;
        result := s; }
    end
  } with result
