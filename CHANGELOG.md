# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Compatible Versioning](https://github.com/staltz/comver).

## [Unreleased]

## [4.0] - 2017-11-30

## [3.0] - 2017-11-25

## [2.1] - 2017-10-08

## [2.0] - 2017-10-08

## [1.0] - 2017-10-03

## [0.5] - 2017-09-26

## [0.4] - 2017-09-26

## [0.3] - 2017-09-24

## [0.2] - 2017-05-15

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
