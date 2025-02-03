# winasyncsocket: Cross-platform asynchronous socket I\/O

This package provides low-level bindings for asynchronous socket I\/O across all major platforms, integrating with each platform's native I\/O facilities:

- Windows: Windows Sockets 2 (Winsock2) with I\/O Completion Ports (IOCP)
- Linux: epoll
- macOS/BSD: kqueue

## Naming

While the package is named `winasyncsocket` due to its initial focus on Windows, it now supports all major platforms with their native asynchronous I\/O mechanisms. The name is retained for historical reasons.

## Features

- Native integration with platform-specific I\/O facilities:
  - Windows: IOCP
  - Linux: epoll
  - macOS/BSD: kqueue
- Direct integration with GHC's I\/O manager
- Support for both IPv4 and IPv6
- Asynchronous TCP operations (accept, connect, send, receive)
- Platform-specific optimizations and features when available

## Compatibility Note

While some function names match those in the "network" package, this package is not a drop-in replacement. The API is designed to leverage platform-specific asynchronous I\/O models for maximum performance.

## Contributing

The package welcomes contributions in these areas:

- UDP support
- Bug reports and fixes
- Extended test coverage
- Documentation improvements
- Platform-specific optimizations
- Performance benchmarks and comparisons

Please feel free to submit issues and pull requests.
