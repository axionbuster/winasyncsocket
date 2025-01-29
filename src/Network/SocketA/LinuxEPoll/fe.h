// fe.h -- BSD-3-Clause
// Copyright (C) axionbuster 2025

#ifndef FE_H
#define FE_H

// feature test macros / standard functions

// expose XSI-compliant strerror_r
#define _POSIX_C_SOURCE 200809L
#undef _GNU_SOURCE
#include <string.h>

// XSI-compliant strerror_r
inline int hs_strerror_r1 (int e, char *b, size_t l) {
  return strerror_r(e, b, l);
}

#endif // FE_H
