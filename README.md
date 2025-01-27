# winasyncsocket: Truly asynchronous socket I\/O on Windows

This package provides low-level bindings to Windows Sockets 2 (Winsock2) API with proper integration into GHC's I\/O manager. It enables truly asynchronous network operations on Windows by utilizing I\/O Completion Ports (IOCP) and Windows socket extension functions.

## Features

- Direct integration with GHC's I\/O manager via IOCP
- Support for both IPv4 and IPv6
- Asynchronous TCP operations (accept, connect, send, receive)
- Raw access to Winsock2 API when needed

## Compatibility Note

While some function names match those in the "network" package, this package is not a drop-in replacement. The API is designed specifically for Windows and its asynchronous I\/O model, which differs significantly from POSIX systems.

## Contributing

The package is in early development and needs:

- More extensive testing
- Documentation improvements
- Additional Windows-specific socket features
- Performance benchmarks

Contributions in any of these areas are very welcome. Please feel free to submit issues and pull requests.
