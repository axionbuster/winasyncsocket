# winasyncsocket: Truly asynchronous socket I/O on Windows and Linux

This package provides low-level bindings to Windows Sockets 2 (Winsock2) API with proper integration into GHC's I/O manager. It enables truly asynchronous network operations on Windows by utilizing I/O Completion Ports (IOCP) and Windows socket extension functions. Additionally, it supports asynchronous socket operations on Linux using epoll.

## Naming

The package is named `winasyncsocket` because it was initially developed to provide asynchronous socket I/O on Windows. However, it also supports Linux (via epoll), and will additionally support macOS/BSD (via kqueue), and the name is kept for historical reasons.

## Features

- Direct integration with GHC's I/O manager via IOCP (Windows) and epoll (Linux)
- Support for both IPv4 and IPv6
- Asynchronous TCP operations (accept, connect, send, receive)
- Raw access to Winsock2 API on Windows and epoll on Linux when needed

## Compatibility Note

While some function names match those in the "network" package, this package is not a drop-in replacement. The API is designed specifically for Windows and Linux asynchronous I/O models, which differ significantly from POSIX systems.

## Future Plans

- Support for macOS/BSD using kqueue

## Contributing

The package is in early development and needs:

- More extensive testing
- Documentation improvements
- Additional platform-specific socket features
- Performance benchmarks

Contributions in any of these areas are very welcome. Please feel free to submit issues and pull requests.
