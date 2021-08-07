# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [1.1.1] — 2021-08-07

### Fixed

- Fixed a bug (regression) which caused `julia-snail-send-region' to only work on regions containing a single expression.


## [1.1.0] — 2021-08-06

### Added

- Support for remote REPLs through SSH and Tramp.
- Support for the Julia multimedia interface (i.e. plots) in graphical Emacs builds.


### Fixed

- Source location for separately evaluated top-level forms or regions now points to the original source, instead of temporary files ([#28](https://github.com/gcv/julia-snail/issues/28)).
- Improved compatibility with Revise.


## [1.0.0] — 2021-03-30

First reasonably stable release.


## [1.0.0beta6] — 2020-03-15

First public release.
