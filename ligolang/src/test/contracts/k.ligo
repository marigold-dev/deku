function k (const x : int; const _y : int) is x

function k2 (const x : int; const _ : int) is x

const m = case Some (4) of
    Some (_x) -> 1
  | None -> 0
  end

const m2 = case Some (4) of
    Some (_) -> 1
  | None -> 0
  end
