function t (const x : list(int)) is
  case x of
  | hd#(hd2#tl) -> hd + hd2
  | nil -> 0
  end