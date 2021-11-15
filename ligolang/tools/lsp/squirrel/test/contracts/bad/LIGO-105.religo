#if DEBUG
#elif VERBOSE
#else
#endif

#include "file"

#define FOO
#define BAR

#undef FOO

#if BAR
#  error Unsupported
#endif
