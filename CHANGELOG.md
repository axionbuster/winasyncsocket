# Changelog for `winsocktest2`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.2.0.0 - 2025-02-03

- Integrated with [unliftio-core] for better monad transformer support.
  Feature flag `unliftio` is available to enable this feature.
  The flag is enabled by default.
- Bump `base` lower bound from `4.7` (GHC 7) to `4.17` (GHC 9.4.8).
- Fixed bug in `sendall` function on POSIX platforms
- Made `TCPIP` modules internal
- Added documentation improvements

[unliftio-core]: https://hackage.haskell.org/package/unliftio-core

## 0.1.0.0 - 2025-02-02

- First draft of the package
- TCP echo server and client
- Unified API for all platforms
- Support Windows, Linux, macOS, and BSD
