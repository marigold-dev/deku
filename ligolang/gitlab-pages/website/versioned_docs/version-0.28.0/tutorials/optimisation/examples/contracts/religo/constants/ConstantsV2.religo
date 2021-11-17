let main = ((parameter, storage): (int, unit)) => {
  if (parameter < 100) {
    ([] : list(operation), ())
  } else {
    (failwith("PARAM_TOO_LARGE") : (list(operation), unit))
  }
};
