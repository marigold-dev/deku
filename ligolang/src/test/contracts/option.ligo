// Test the option type in PascaLIGO

type foobar is option (int)

const s : foobar = Some (42)
const n : foobar = None
const i : int = Option.unopt (Some (42))

function assign (var m : int) : foobar is
  block {
    var _coco : foobar := None;
    _coco := Some (m);
    _coco := (None : foobar); //temporary annotation added until type inference
  } with _coco
