// Test while loops in PascaLIGO

function inner_capture_in_conditional_block (var _ : unit) : bool * int is
  block {
    var count : int := 1;
    var ret : bool := False;
    var mylist : list (int) := list [1; 2; 3];
    for it1 in list mylist block {
      for it2 in list mylist block {
        if count = it2 then ret := not (ret) else skip
      };
      count := count + 1
    }
  } with (ret, count)
