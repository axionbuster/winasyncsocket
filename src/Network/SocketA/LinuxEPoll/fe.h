// fe.h -- BSD-3-Clause
// Copyright (C) axionbuster 2025

#ifndef FE_H
#define FE_H

// feature test macros / standard functions

// expose XSI-compliant strerror_r
#include <string.h>

// XSI-compliant strerror_r
inline int hs_strerror_r1 (int e, char *b, size_t l) {
#if defined(__GLIBC__)
  // bug on some systems: GNU-specific strerror_r gets used
  // instead of XSI-compliant strerror_r despite _POSIX_C_SOURCE
  // and _GNU_SOURCE not being defined
  char *s = strerror_r(e, b, l);
  if (!s) return -1;
  if (s == b) return 0;
  strncpy(b, s, l);
  return 0;
#else
  return strerror_r(e, b, l);
#endif
}

#endif // FE_H
