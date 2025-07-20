Change log for `time-hourglass`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.2.12

* Rename `hourglass-0.2.12` package as `time-hourglass-0.2.12`.
* Change maintainer field to `Mike Pilgrem <public@pilgrem.com>`.
* Reset `CHANGELOG.md`.
* In test-suite `test-hourglass` replace use of `parseTime` (removed from
  package `time-1.10`) with `parseTimeM True`.
