type foobar = int option

let s : foobar = Some 42
let n : foobar = None
let i : int    = Option.unopt (s)

let assign (m : int) : foobar =
   let _coco : foobar = None in
   let _coco = Some (m) in
   let coco = (None : foobar) in
   coco
