// Test while loops in PascaLIGO

function nested_for_collection_local_var (var _ : unit) : int*string is
  block {
    var myint : int := 0;
    var myst : string := "";
    var mylist : list (int) := list [1; 2; 3];
    for i in list mylist block {
      var myst_loc : string := "";
      myint := myint + i;
      var myset : set (string) := set ["1"; "2"; "3"];
      for st in set myset block {
        myint := myint + i;
        myst_loc := myst_loc ^ st;
      };
      myst := myst_loc ^ myst
    }
  } with (myint, myst)
