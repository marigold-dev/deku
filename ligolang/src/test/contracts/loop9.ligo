// Test while loops in PascaLIGO

function for_collection_map_kv (var _ : unit) : int * string is
  block {
    var acc : int := 0;
    var st : string := "";
    var mymap : map (string, int) := map ["1" -> 1; "2" -> 2; "3" -> 3];
    for k -> v in map mymap block {
      acc := acc + v;
      st := st ^ k;
    }
  } with (acc, st)
