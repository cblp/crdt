# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Compatible Versioning](https://github.com/staltz/comver).

## [Unreleased]

[Unreleased]: https://github.com/cblp/crdt/compare/4.0...master

## [4.0] - 2017-11-30

[4.0]: https://github.com/cblp/crdt/compare/3.0...4.0

## [3.0] - 2017-11-25

[3.0]: https://github.com/cblp/crdt/compare/2.1...3.0

## [2.1] - 2017-10-08

[2.1]: https://github.com/cblp/crdt/compare/2.0...2.1

## [2.0] - 2017-10-08

[2.0]: https://github.com/cblp/crdt/compare/1.0...2.0

## [1.0] - 2017-10-03

[1.0]: https://github.com/cblp/crdt/compare/0.5...1.0

## [0.5] - 2017-09-26

[0.5]: https://github.com/cblp/crdt/compare/0.4...0.5

## [0.4] - 2017-09-26

[0.4]: https://github.com/cblp/crdt/compare/0.3...0.4

## [0.3] - 2017-09-24

[0.3]: https://github.com/cblp/crdt/compare/0.2...0.3

## [0.2] - 2017-05-15

[0.2]: https://github.com/cblp/crdt/compare/0.1...0.2

## [0.1] - 2017-05-15
### Added
- Hackage package `crdt`
- Classes:
  - `CmRDT`
  - `CvRDT`
- Types:
  - `GCounter` for G-counter:
    - Cm variant
    - Cv variant
  - `LWW`
  - `PNCounter` for PN-counter:
    - Cm variant
    - Cv variant
- Tests:
  - CmRDT law
  - CvRDT laws
  - `GCounter`:
    - increment
  - `LWW`
  - `PNCounter`:
    - increment
    - decrement

[0.1]: https://github.com/cblp/crdt/tree/0.1
