// Test while loops in PascaLIGO

function for_collection_if_and_local_var (var _ : unit) : int is
  block {
    var acc : int := 0;
    const theone : int = 1;
    const thetwo : int = 2;
    var myset : set (int) := set [1; 2; 3];
    for x in set myset block {
      if x = theone then acc := acc + x
      else if x = thetwo then acc := acc + thetwo
      else acc := acc + 10
    }
  } with acc
