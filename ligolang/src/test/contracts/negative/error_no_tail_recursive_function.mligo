let rec unvalid (n:int):int =
    let res = unvalid (n) in
    res + 1
