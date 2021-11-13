let a = 1

let b =
  List.map
    (fun (c : int) -> let d = 1 in d + c + a)
    (let e = 1 in [ e+a  ; e+a ])