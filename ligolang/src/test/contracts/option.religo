type foobar = option (int);

let s : foobar = Some (42);
let n : foobar = None;
let i : int    = Option.unopt (s);


let assign = (m : int) : foobar =>
   let _coco : foobar = None;
   let _coco = Some (m);
   let coco = (None : foobar);
   coco
