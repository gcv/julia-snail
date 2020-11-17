# Julia Snail

![img](snail.png)

Snail is a development environment and REPL interaction package for Julia in the spirit of Common Lisp’s [SLIME](https://common-lisp.net/project/slime/) and Clojure’s [CIDER](https://cider.mx). It enables convenient and dynamic REPL-driven development.


## Features

- **REPL display:** Snail uses [libvterm](https://github.com/neovim/libvterm) with [Emacs bindings](https://github.com/akermu/emacs-libvterm) to display Julia’s native REPL in a good terminal emulator. As a result, the REPL has good performance and far fewer display glitches than attempting to run the REPL in an Emacs-native `term.el` buffer.
- **REPL interaction:** Snail provides a bridge between Julia code and a Julia process running in a REPL. The bridge allows Emacs to interact with and introspect the Julia image. Among other things, this allows loading entire files and individual functions into running Julia processes.
- **Cross-referencing:** Snail is integrated with the built-in Emacs [xref](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html) system. When a Snail session is active, it supports jumping to definitions of functions and macros loaded in the session.
- **Completion:** Snail is also integrated with the built-in Emacs [completion-at-point](https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html) facility. Provided it is configured with the `company-capf` backend, [company-mode](http://company-mode.github.io/) completion will also work (this should be the case by default).
- **Parser:** Snail uses [CSTParser](https://github.com/julia-vscode/CSTParser.jl), a full-featured Julia parser, to infer the structure of source files and to enable features which require an understanding of code context, especially the module in which a particular piece of code lives. This enables awareness of the current module for completion and cross-referencing purposes.


## Demo

![img](https://github.com/gcv/julia-snail/wiki/screencasts/screencast-2020-01-26.gif)


## Installation

Julia versions 1.0–1.4 work. No packages need to be installed on the Julia side (other than Julia itself).

On the Emacs side:

1. Make sure you have Emacs 26.2 or later, compiled with module support (`--with-modules`). Check the value of `module-file-suffix`: it should be non-nil. (This is currently a default compile-time option Emacs distributed with [Homebrew](https://formulae.brew.sh/formula/emacs).)
2. Install [libvterm](https://github.com/neovim/libvterm). It is available in [Homebrew](https://formulae.brew.sh/formula/libvterm) and [Ubuntu 19.10](https://packages.ubuntu.com/eoan/libvterm-dev), and in source form on other systems.
3. Install [emacs-libvterm](https://github.com/akermu/emacs-libvterm) using your Emacs package manager. It is available from [MELPA](https://melpa.org/#/vterm) as `vterm`, so use something like `(package-install 'vterm)` or `(use-package vterm)`. **It is important to do this step separately from the `julia-snail` installation, as you may run into problems with the Emacs package manager and byte-compiler!**
4. Verify that `vterm` works by running `M-x vterm` to start a shell. It should display a nice terminal buffer. You may find it useful to customize and configure `vterm`.
5. Install `julia-snail` using your Emacs package manager (see below for a sample `use-package` invocation). It is available on [MELPA](https://melpa.org/#/julia-snail) and [MELPA Stable](https://stable.melpa.org/#/julia-snail).

Optionally, install [markdown-mode](https://github.com/jrblevin/markdown-mode) to improve documentation buffer display.


## Configuration

### `use-package` setup

**Make sure to install vterm first!** (See the [Installation](#installation) section.)

```elisp
(use-package julia-snail
  :hook (julia-mode . julia-snail-mode))
```


### Manual setup

```elisp
(add-to-list 'load-path "/path/to/julia-snail")
(require 'julia-snail)
(add-hook 'julia-mode-hook #'julia-snail-mode)
```

Then configure key bindings in `julia-snail-mode-map` as desired.


### `display-buffer-alist` notes

Window and buffer display behavior is one of the worst defaults Emacs ships with. [Please refer to remarks on the subject written elsewhere](https://github.com/nex3/perspective-el/#some-musings-on-emacs-window-layouts). Some packages go to great lengths to provide clean custom window management (e.g., Magit), but Snail does not have the resources to implement such elaborate schemes.

Snail uses the Emacs `display-buffer` system to pop up windows, and tries to do so in the most sane manner possible. Most information pop-ups (except the REPL) can be dismissed by pressing `q`, and this should restore the window configuration in most cases. With Emacs defaults, Snail should also reuse existing windows as much as possible, i.e., calling `julia-snail` from a source buffer will switch an already-visible REPL window, and calling `julia-snail` from a REPL window will switch back to a source buffer and reuse already-visible windows.

However, `display-buffer` will (by default) split windows if the target buffer is not visible. To prevent splits altogether, try the following:

```
(add-to-list 'display-buffer-alist
             '("\\*julia" (display-buffer-reuse-window display-buffer-same-window)))
```

(If you like this setting, and find other Emacs packages’ splitting irritating, consider replacing the `"\\*julia"` regexp with `".*"`: this will suppress pretty much all window splits.)

It is likely that most users will want the default REPL pop-up behavior to split the window vertically, but the default `split-window-sensibly` implementation only splits that way if `split-height-threshold` is smaller than the current window height. `split-height-threshold` defaults to 80 (lines), and relatively few windows will be that tall. Therefore, consider adding something like the following to your configuration:

```elisp
(setq split-height-threshold 15)
```


## Usage

### Basics

Once Snail is properly installed, open a Julia source file. If `julia-mode-hook` has been correctly configured, `julia-snail-mode` should be enabled in the buffer (look for the Snail lighter in the modeline).

Start a Julia REPL using `M-x julia-snail` or `C-c C-z`. This will load all the Julia-side supporting code Snail requires, and start a server. The server runs on a TCP port (10011 by default) on localhost. You will see `JuliaSnail.start(<port>)` execute on the REPL.

The REPL buffer uses `libvterm` mode, and `libvterm` configuration and key bindings will affect it.

If the Julia program uses Pkg, then run `M-x julia-snail-package-activate` or `C-c C-a` to enable it. (Doing this using REPL commands like `]` also works as normal.)

Load the current Julia source file using `M-x julia-snail-send-buffer-file` or `C-c C-k`. Notice that the REPL does not show an `include()` call, because the command executed across the Snail network connection. Among other advantages, this minimizes REPL history clutter.

Once some Julia code has been loaded into the running image, Snail can begin introspecting it for purposes of cross-references and identifier completion.

The `julia-snail-mode` minor mode provides a key binding map (`julia-snail-mode-map`) with the following commands:

[//]: # (The table is formatted to look reasonable in GitHub, which limits text width and makes wide columns wrap horribly.)

| key     | command and description                                                                                            |
|---------|--------------------------------------------------------------------------------------------------------------------|
| C-c C-z | julia-snail                     <br> _start a REPL; flip between REPL and source_                                  |
| C-c C-a | julia-snail-package-activate    <br> _activate the project using `Project.toml`_                                   |
| C-c C-d | julia-snail-doc-lookup          <br> _display the docstring of the identifier at point_                            |
| C-c C-c | julia-snail-send-top-level-form <br> _evaluate `end`-terminated block around the point in the current module_      |
| C-M-x   | julia-snail-send-top-level-form <br> _ditto_                                                                       |
| C-c C-r | julia-snail-send-region         <br> _evaluate active region in the current module (or in `Main` with prefix arg)_ |
| C-c C-l | julia-snail-send-line           <br> _copy current line directly to REPL_                                          |
| C-c C-e | julia-snail-send-dwim           <br> _if region active, evaluate it in current module; <br> else if on top-level block, evaluate it in current module; <br> else copy line to REPL_ |
| C-c C-k | julia-snail-send-buffer-file    <br> _`include()` the current buffer’s file_                                       |
| C-c C-R | julia-snail-update-module-cache <br> _update module-nested `include` cache (mainly for Revise)_                    |

Several commands include the note “in the current module”. This means the Julia parser will determine the enclosing `module...end` statements, and run the relevant code in that module. If the module has already been loaded, this means its global variables and functions will be available.

In addition, most `xref` commands are available (except `xref-find-references`). `xref-find-definitions`, by default bound to `M-.`, does a decent job of jumping to function and macro definitions. Cross-reference commands are current-module aware.

Completion also works. Emacs built-in completion features, as well as `company-complete`, will do a reasonable job of finding the right completions in the context of the current module (though will not pick up local variables). Completion is current-module aware.


### Multiple REPLs

To use multiple REPLs, set the local variables `julia-snail-repl-buffer` and `julia-snail-port`. They must be distinct per-project. They can be set at the [file level](https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html), or at the [directory level](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html). The latter approach is recommended, using a `.dir-locals.el` file at the root of a project directory.

For example, consider two projects: `Mars` and `Venus`, both of which you wish to work on at the same time. They live in different directories.

The `Mars` project directory contains the following `.dir-locals.el` file:

```elisp
((julia-mode . ((julia-snail-port . 10050)
                (julia-snail-repl-buffer . "*julia Mars*"))))
```

The `Venus` project directory contains the following `.dir-locals.el` file:

```elisp
((julia-mode . ((julia-snail-port . 10060)
                (julia-snail-repl-buffer . "*julia Venus*"))))
```

(Be sure to refresh any buffers currently visiting files in `Mars` and `Venus` using `find-alternate-file` or similar after changing these variables.)

Now, source files in `Mars` will interact with the REPL running in the `*julia Mars*` buffer, and source files in `Venus` will interact with the REPL running in the `*julia Venus*` buffer.


### Multiple Julia versions

The `julia-snail-executable` variable can be set at the file level or at the directory level and point to different versions of Julia for different projects. It should be a string referencing the executable binary path.

NB: On a Mac, the Julia binary is typically `Contents/Resources/julia/bin/julia` inside the distribution app bundle. You must either make sure `julia-snail-executable` is set to an absolute path, or configure your Emacs `exec-path` to correctly find the `julia` binary.


### Extra Julia command-line arguments

The `julia-snail-extra-args` variable can be set to include additional arguments to the Julia binary. It can be set to `nil` (the default), a string, or a list of strings.

This variable is buffer-local, so it can be kept distinct per-project using `.dir-locals.el`. The following example starts Julia with a custom image and automatically activates a specific project:

```elisp
((julia-mode . ((julia-snail-extra-args . ("--sysimage" "/path/to/image"
                                           "--project=/path/to/project")))))
```


### Module-nested `include`s

Consider the following file, call it `alpha.jl`:

```julia
module Alpha

include("alpha-1.jl")
include("alpha-2.jl")

end
```

Everything in the files `alpha-1.jl` and `alpha-2.jl` is inside the `Alpha` module, but neither of these files will mention that module explicitly. Snail supports this by using the Julia parser to track `include(...)` calls and their module context. This feature works with nested modules.

Using this feature requires some care. The root file which contains the module declaration (`alpha.jl` in this example) must be loaded using `julia-snail-send-buffer-file` first (or, for [Revise](https://github.com/timholy/Revise.jl) users, `julia-snail-update-module-cache`). If this does not happen, the parser will not have the opportunity to learn where `alpha-1.jl` and `alpha-2.jl` fit in the module hierarchy, and will assume their parent module is `Main`. The same applies to any deeper nesting of files (i.e., if `alpha-1.jl` then does `include("alpha-1.1.jl")`, then `julia-snail-send-buffer-file` or `julia-snail-update-module-cache` must be executed from `alpha-1.jl`).

Furthermore, if `alpha-1.jl` is refactored to sit outside the `Alpha` module, or moved in the directory structure, Snail must be informed. To do this, call the `julia-snail-clear-caches` command.


### Documentation lookup

`julia-snail-doc-lookup` shows the documentation string of the identifier at point. If the current Emacs session has [markdown-mode](https://github.com/jrblevin/markdown-mode) installed, it will be turned on with markup hiding enabled.


## Future improvements

### Foundational

- The Julia interaction side of the Snail server is single-threaded (using `@async`). This means the interaction locks up while the REPL is working or running code. Unfortunately, Julia as of version 1.2 does not have user-accessible low-level multithreading primitives necessary to implement a truly multi-threaded Snail server. The newer `Threads.@spawn` macro needs to be investigated.


### Structural

-  The `libvterm` dependency forces the use of very recent Emacs releases, forces Emacs to be build with module support, complicates support for Windows, and is generally quite gnarly. It would be much better to re-implement the REPL in Elisp.


### Functional

- Completion does not pick up local variables.
- A real eldoc implementation would be great, but difficult to do with Julia’s generic functions.
- A debugger would be great.
