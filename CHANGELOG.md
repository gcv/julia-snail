# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [1.1.5] — 2022-02-17

### Fixed

- When a Julia-side dependency of Snail (like CSTParser) has already been installed in the Julia global environment, it can cause conflicts (see [#62](https://github.com/gcv/julia-snail/issues/62)). Work around this problem by forcing the Snail Julia project to be first in `LOAD_PATH` order, but only during initial Snail load.
- Local REPLs should start with the same working directory as the starting file ([#69](https://github.com/gcv/julia-snail/issues/69)).
- A bug that kept the spinner running in a source buffer when a multimedia image about to be displayed ([#73](https://github.com/gcv/julia-snail/pull/73)).


### Added

- Support for running Julia instances in Docker containers using Tramp.
- Support for evaluating code in notebook-style code-cells (https://github.com/astoff/code-cells.el).
- Support for Snail extensions. See [README](https://github.com/gcv/julia-snail#extensions) and [EXTENSIONS](https://github.com/gcv/julia-snail/blob/master/EXTENSIONS.md) files for details.
- New extension: `repl-history`, which allows searching and yanking REPL history in source buffers.
- New extension: `formatter`, a wrapper for [JuliaFormatter.jl](https://github.com/domluna/JuliaFormatter.jl).


### Changed

- The modeline lighter now shows a 🐌 emoji instead of the string `"Snail"` (unless `julia-snail-use-emoji-mode-lighter` is `nil` or overriden elsewhere). Excellent idea from the discussion in [#70](https://github.com/gcv/julia-snail/issues/70).


## [1.1.4] — 2021-08-17

### Fixed

- Some versions of [`emacs-libvterm`](https://github.com/akermu/emacs-libvterm) notice that `default-directory` of a buffer points to a remote host and hijack the `ssh` invocation. Prevent this from conflicting with Snail's own use of `ssh`.


## [1.1.3] — 2021-08-12

No functionality changes. Only documentation updates.


## [1.1.2] — 2021-08-11

### Fixed

- Fixed a potential bug if there is a different default username configured for the ssh remote host in `.ssh/config` from the one used in the Tramp connection string (i.e., if `.ssh/config` says `myhost` should use default username `myname1` but the Tramp connection string is `/ssh:myname2@myhost:`).


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
