#include <guiddef.h>
#include <mswsock.h>

// WSAID_ACCEPTEX is sort of an exotic kind of macro/expression,
// so we need to get it in such a roundabit way.
void hs_getwsaidacceptex(GUID *g) { GUID h = WSAID_ACCEPTEX; *g = h; }
