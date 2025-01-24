// ax.h -- BSD-3-Clause
// Copyright (C) axionbuster 2025

#ifndef AX_H
#define AX_H

#include <guiddef.h>

// what function is to be fetched
enum HSGUIDENUM {
  HS_ACCEPTEX,
  HS_CONNECTEX,
  HS_DISCONNECTEX,
  HS_GETACCEPTEXSOCKADDRS,
  HS_TRANSMITFILE,
  HS_TRANSMITPACKETS,
  HS_WSARECVMSG,
  HS_WSASENDMSG
};

// GUID that will be fetched
//
// SAFETY: none; the caller must ensure that the GUID option is valid;
// if not, undefined behavior will be invoked
void hs_getguid(GUID *, enum HSGUIDENUM);

#endif /* AX_H */
