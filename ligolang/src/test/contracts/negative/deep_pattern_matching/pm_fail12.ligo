type recordi is record [ a : option(list(int)) ; b : list(int) ]

function t (const x:recordi) is
  case x of
  | record [ a = Some (nil) ; b = (hd#tl) ] -> hd
  | record [ a = Some ((hd#tl)) ; b = nil ] -> hd
  end