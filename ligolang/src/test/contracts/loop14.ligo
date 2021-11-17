// Test while loops in PascaLIGO

function for_collection_comp_with_acc (var _ : unit) : int is
  block {
    var myint : int := 0;
    var mylist : list (int) := list [1; 10; 15];
    for x in list mylist block {
      if x < myint then skip;
      else myint := myint + 10
    }
  } with myint
