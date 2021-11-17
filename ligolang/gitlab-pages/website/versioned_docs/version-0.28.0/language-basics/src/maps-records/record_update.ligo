type point is record [x : int; y : int; z : int]
type vector is record [dx : int; dy : int]

const origin : point = record [x = 0; y = 0; z = 0]

function xy_translate (var p : point; const vec : vector) : point is
  p with record [x = p.x + vec.dx; y = p.y + vec.dy]
