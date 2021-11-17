// Test while loops in PascaLIGO

function for_collection_rhs_capture (var _ : unit) : int is
  block {
    var acc : int := 0;
    const mybigint : int = 1000;
    var myset : set (int) := set [1; 2; 3];
    for x in set myset block {
      if x = 1 then acc := acc + mybigint
      else acc := acc + 10
    }
  } with acc

function for_collection_proc_call (var _ : unit) : int is
  block {
    var acc : int := 0;
    var myset : set (int) := set [1; 2; 3];
    for x in set myset block {
      if x = 1 then
        acc := acc + for_collection_rhs_capture (unit)
      else acc := acc + 10
    }
  } with acc
