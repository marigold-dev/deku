function t (const x : list(int)) is
  case x of
  | a -> 0
  | hd#tl -> 0
  end