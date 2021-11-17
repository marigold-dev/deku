function main (const parameter : bytes; const storage : int) is
block {
  const nop : list (operation) = list []
} with
    if (parameter = 0xbc1ecb8e)
    then (nop, storage + 1)
    else
      if (parameter = 0x36e44653)
      then (nop, storage - 1)
      else (failwith ("Unknown entrypoint") : list (operation) * int)
