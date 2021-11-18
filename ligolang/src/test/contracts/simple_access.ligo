//Test simple_access in PascaLIGO

type tpi is int * int

type rpi is record [x : int; y : int]

type mpi is map (string, int)

function main (const toto : tpi) : int is
  block {
    var a : tpi := toto;
    var b : rpi := record [x=0; y=1];
    var m : mpi := map ["y" -> 1];
    a.0 := 2;
    b.x := a.0;
    m["x"] := b.x
  } with
      case m["x"] of
        Some (s) -> s
      | None -> 42
      end
