// Test while loops in PascaLIGO

function for_collection_with_patches (var _ : unit) : map (string,int) is
  block {
    var myint : int := 12;
    var mylist : list (string) := list ["I"; "am"; "foo"];
    var mymap : map (string,int) := map [];
    for x in list mylist block {
      patch mymap with map [x -> myint]
    }
  } with mymap
