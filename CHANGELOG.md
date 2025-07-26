Change log for `time-hourglass`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## UNRELEASED

* Drop deprecated modules `Data.Hourglass.Compat`, `Data.Hourglass.Epoch`,
  `Data.Hourglass.Types` and `System.Hourglass`. Use modules `Time.Compat`,
  `Time.Epoch` and `Time.Types`.
* Drop deprecated function `dateFromPOSIXEpoch`. Use `dataFromUnixEpoch`.
* Drop deprecated function `dateFromTAIEpoch`. Use `dateFromMJDEpoch`.
* Add `Real` instance for `ElapsedSinceP`.
* Fix Haddock documentaton for `Format_Hours`, `Format_Minutes` and
  `Format_Seconds`; they all pad to 2 characters.
* Fix Haddock documentaton for `Format_Millisecond`, `Format_MicroSecond` and
  `Format_NanoSecond`; they parse and print components only, and all pad to 3
  characters.
* Fix error message if a `Format_Text` parse fails.
* Add Haddock documentation for the `String` instance of `TimeFormat`.

## 0.2.14

* In test-suite and benchmark, depend on main library, drop dependency on
  package `hourglass`.

## 0.2.13

* Drop support for GHC < 8.4.
* Move library modules to directory `src` and benchmark module to directory
  `benchmarks`.
* Move module `Example.Time.Compat` to directory `examples`.
* Expose module `Time.Epoch` and deprecate equivalent module
  `Data.Hourglass.Epoch`.
* Renamed non-exposed library modules under the `Time.*` hierarchy.
* Use `LANGUAGE PackageImports` in module `Example.Time.Compat`, allowing
  `stack ghci examples/Example/Time/Compat.hs`.
* Eliminate the use of CPP to vary source code for different operating systems.
* Fix `other-modules` of `bench-hourglass` benchmark.
* `bench-hourglass` benchmark depends on `tasty-bench`, drop dependency on
  `gauge`.
* Improve Haddock and other documentation.
* Export new `dateFromUnixEpoch` and deprecate identical `dateFromPOSIXEpoch` to
  name epoch consistently.
* Export new `dateFromMJDEpoch` and deprecate identical `dateFromTAIEpoch` to
  fix the latter being a misnomer.

## 0.2.12

* Rename `hourglass-0.2.12` package as `time-hourglass-0.2.12`.
* Cabal file specifies `cabal-version: 1.12` (not `>= 1.10`).
* Change maintainer field to `Mike Pilgrem <public@pilgrem.com>`.
* Add `bug-reports` field to Cabal file.
* Reset `CHANGELOG.md`.
* Update `README.md` badges.
* In test-suite `test-hourglass` replace use of `parseTime` (removed from
  package `time-1.10`) with `parseTimeM True`.
