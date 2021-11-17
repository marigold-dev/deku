let main = ((parameter, storage): (int, unit)) => {
  if (parameter < 100) {
    ([] : list(operation), ())
  } else {
    (failwith("The passed parameter is too large, consider passing a value less than 100")
      : (list(operation), unit))
  }
};
