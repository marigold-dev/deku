// Test while loops in PascaLIGO

function for_collection_set (var _ : unit) : int * string is
  block {
    var acc : int := 0;
    var st : string := "to";
    var myset : set (int) := set [1; 2; 3];
    for x in set myset block {
      acc := acc + x;
      st := st ^ "to"
    }
  } with (acc, st)
