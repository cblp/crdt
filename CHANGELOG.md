# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Compatible Versioning](https://github.com/staltz/comver).

## [Unreleased]
### Added
- Support for GHC 8.4

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
### Added
- CRDTs:
  - `CRDT.Cm.Counter` for the op-based counter.
    - Law test.
  - `CRDT.Cm.GSet` for the op-based G-set.
  - Op-based LWW with `Assign` op.
  - `CRDT.Cm.TPSet` for the op-based 2P-set.
    - Law test.
  - `CRDT.Cv.Max`.
    - Law test.
- Class `Observe`.
- Module `LamportClock` to work with Lamport clock simulation:
  - Types `Time`, `Timestamp`, `Pid`, `LamportClock`, `Process`.
  - Class `Clock`.
  - Functions `barrier`, `runLamportClock`, `runProcess`.
- Module `Lens.Micro.Extra` with lens helpers.

### Changed
- Reorganized modules:
  - Grouped into two groups: `CRDT.Cm` for Cm types and `CRDT.Cv` for Cv ones.
  - Removed `Internal` submodules;
    all guts are exported until it will become an issue.
- `CmRDT` class:
  - Made it parameterized by 3 types: _payload_, _op_ and _update_.
  - Used `PartialOrd` (from `lattices:Algebra.PartialOrd`) of ops
    as a prerequisite.
  - Used `Observe` to compare only user-visible parts of CmRDT payload.
  - Renamed `update` to `updateDownstream` to be closer to the paper.
  - Added `updateAtSource` as written in the paper.
    - Added its precodition as the separate method `updateAtSourcePre`.
  - Allowed updates to be run in a `Clock`-constrained monad to get timestamps.
- LWW:
  - Cv variant:
    - Made `initial` and `assign` dependent on `Clock` monad
      since they need timestamps and cannot rely on user-provided timestamps.
- Module `Data.Semilattice`:
  - Renamed Semilattice specialization of Semigroup's `(<>)` from `slappend`
    to `merge` to show symmetry.
- Moved `Arbitrary` orphan instances from `Instances` module into
  `ArbitraryOrphans` module.
- CmRDT law test:
  - Used Lamport clock to check ops and update payloads at source.

[1.0]: https://github.com/cblp/crdt/compare/0.5...1.0

## [0.5] - 2017-09-26
### Added
- Exported `GSet` type.

### Changed
- Cabal-file:
  - Shorten `copyright` section.

[0.5]: https://github.com/cblp/crdt/compare/0.4...0.5

## [0.4] - 2017-09-26
### Added
- Travis config.
- HLint config.
- README.
- CRDTs:
  - `GSet` for G-set.
- `Timestamp` type for simple natural timestamps.
- In module `CRDT.LWW`:
  - Functions for LWW:
    - `point`.
    - `write`.
    - `query`.
- Nikolay Loginov as an author.
- Tests:
  - `GCounter`:
    - CmRDT variant:
      - Law.
      - Increment.
    - CvRDT variant:
      - Laws.
      - Increment.
  - `GSet`:
    - CvRDT laws.
    - Add.
  - `LWW`:
    - CmRDT instance:
      - Law.
      - Write latter.
      - Write former.
    - CvRDT instance:
      - Laws.
      - Write latter.
      - Write former.
  - `PNCounter`:
    - CmRDT variant:
      - Law.
      - Increment.
      - Decrement.
    - CvRDT variant:
      - Laws.
      - Increment.
      - Decrement.

### Changed
- Module `Data.Semilattice`:
  - Renamed Semilattice specialization of Semigroup's `(<>)` from `(<>)`
    to `slappend`.
- Moved law tests to the module `Test.Laws`.

### Removed
- Common CmRDT `query` function.

[0.4]: https://github.com/cblp/crdt/compare/0.3...0.4

## [0.3] - 2017-09-24
### Changed
- Changed implemetation of `GCounter` from `Vector` to `IntMap`.

[0.3]: https://github.com/cblp/crdt/compare/0.2...0.3

## [0.2] - 2017-05-15

### Added
- Module `Data.Semilattice`:
  - Class `Semilattice`, the same as `CvRDT` was earlier.

### Changed
- Renamed `CvRDT` class to `Semilattice`.
  Re-added `CvRDT` as an alias to `Semilattice`.
- Renamed tests to reflect that CvRDT = Semilattice.

[0.2]: https://github.com/cblp/crdt/compare/0.1...0.2

## [0.1] - 2017-05-15
### Added
- Hackage package `crdt`.
- Classes:
  - `CmRDT`
  - `CvRDT`
- CRDTs:
  - `GCounter` for G-counter:
    - Cm variant.
    - Cv variant.
  - `LWW`.
  - `PNCounter` for PN-counter:
    - Cm variant.
    - Cv variant.
- Tests:
  - CmRDT law.
  - CvRDT laws.
  - `GCounter`:
    - increment
  - `LWW`.
  - `PNCounter`:
    - Increment.
    - Decrement.
- Stylish-haskell config.

[0.1]: https://github.com/cblp/crdt/tree/0.1
