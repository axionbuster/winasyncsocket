// ax.c -- BSD-3-Clause
// Copyright (C) axionbuster 2025

#include <guiddef.h>
#include <mswsock.h>

#include "ax.h"

#define FETCH(i) { GUID h = i; *g = h; }

void hs_getguid(GUID *g, enum HSGUIDENUM q) {
  switch (q) {
    case HS_ACCEPTEX: FETCH(WSAID_ACCEPTEX); break;
    case HS_CONNECTEX: FETCH(WSAID_CONNECTEX); break;
    case HS_DISCONNECTEX: FETCH(WSAID_DISCONNECTEX); break;
    case HS_GETACCEPTEXSOCKADDRS: FETCH(WSAID_GETACCEPTEXSOCKADDRS); break;
    case HS_TRANSMITFILE: FETCH(WSAID_TRANSMITFILE); break;
    case HS_TRANSMITPACKETS: FETCH(WSAID_TRANSMITPACKETS); break;
    case HS_WSARECVMSG: FETCH(WSAID_WSARECVMSG); break;
    case HS_WSASENDMSG: FETCH(WSAID_WSASENDMSG); break;
    default: (void)(*(char*)NULL); break; // invoke UB to mark unreachable
  }
}

// must be called when AcceptEx finishes accepting a socket
//
// error: inherited from setsockopt; 0 on success, otherwise consult
// WSAGetLastError().
int hs_finishaccept(SOCKET l, SOCKET a) {
  return
    setsockopt(a, SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT, (char *)&l, sizeof l);
}
