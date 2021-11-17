function shadowing_in_body (var _ : unit) : string is block {
  var _st : string := "";
  var list1 : list (string) := list ["to"; "to"];
  for _x in list list1 block {
    const _x : string = "ta";
    _st := _st ^ _x;
  };
} with _st
(* should be "tata" *)

function shadowing_assigned_in_body (var _ : unit) : string is block {
  var _st : string := "";
  var list1 : list (string) := list ["to"; "to"];
  for _x in list list1 block {
    _st := _st ^ _x;
    var _st : string := "ta";
    _st := _st ^ _x;
  };
} with _st
(* should be "toto" ??? *)
