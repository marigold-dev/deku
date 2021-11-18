let main = ((parameter, storage): (bytes, int)) => {
  if (parameter == 0xbc1ecb8e) {
    ([] : list(operation), storage + 1)
  } else {

    if (parameter == 0x36e44653) {
      ([] : list(operation), storage - 1)
    } else {
      (failwith("Unknown entrypoint") : (list(operation), int))
    }
  }
};
