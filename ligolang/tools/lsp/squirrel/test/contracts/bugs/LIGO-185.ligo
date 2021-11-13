#define A
#define B

const x =
#if A
#  if !B
  1
#  else
  2
#  endif
#elif B
  3
#else
  4
#endif
