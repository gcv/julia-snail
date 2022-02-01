# Julia Snail

![img](snail.png)

Snail is a development environment and REPL interaction package for Julia in the spirit of Common Lisp’s [SLIME](https://common-lisp.net/project/slime/) and Clojure’s [CIDER](https://cider.mx). It enables convenient and dynamic REPL-driven development.

Snail works on platforms which support [libvterm](https://github.com/neovim/libvterm), which currently means Unix-like systems. It should also work on Windows using [WSL](https://docs.microsoft.com/en-us/windows/wsl/about).

Refer to the [changelog](https://github.com/gcv/julia-snail/blob/master/CHANGELOG.md) for release notes.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
- [Features](#features)
- [Demo](#demo)
- [Installation](#installation)
- [Configuration](#configuration)
    - [`use-package` setup](#use-package-setup)
    - [Manual setup](#manual-setup)
    - [`display-buffer-alist` notes](#display-buffer-alist-notes)
    - [Other customizations](#other-customizations)
- [Usage](#usage)
    - [Basics](#basics)
    - [Multiple Julia versions](#multiple-julia-versions)
    - [Multiple REPLs](#multiple-repls)
    - [Remote REPLs](#remote-repls)
        - [`julia-snail-executable` and the remote shell path](#julia-snail-executable-and-the-remote-shell-path)
        - [Remote environment setup](#remote-environment-setup)
    - [Docker container REPLs](#docker-container-repls)
    - [Extra Julia command-line arguments](#extra-julia-command-line-arguments)
    - [Module-nested `include`s](#module-nested-includes)
    - [Documentation lookup](#documentation-lookup)
    - [Multimedia and plotting](#multimedia-and-plotting)
    - [`code-cells` integration (notebook mode)](#code-cells-integration-notebook-mode)
- [Extensions](#extensions)
    - [REPL history](#repl-history)
    - [Formatter](#formatter)
- [Future improvements](#future-improvements)
<!-- markdown-toc end -->


## Features

- **REPL display:** Snail uses [libvterm](https://github.com/neovim/libvterm) with [Emacs bindings](https://github.com/akermu/emacs-libvterm) to display Julia’s native REPL in a good terminal emulator. As a result, the REPL has good performance and far fewer display glitches than attempting to run the REPL in an Emacs-native `term.el` buffer.
- **REPL interaction:** Snail provides a bridge between Julia code and a Julia process running in a REPL. The bridge allows Emacs to interact with and introspect the Julia image. Among other things, this allows loading entire files and individual functions into running Julia processes.
- **Remote REPLs:** Julia sessions on remote machines work transparently with Snail using SSH and Emacs Tramp.
- **Multimedia and plotting:** Snail can display Julia graphics in graphical Emacs instances using packages like [Plots](http://juliaplots.org) and [Gadfly](http://gadflyjl.org).
- **Cross-referencing:** Snail is integrated with the built-in Emacs [xref](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html) system. When a Snail session is active, it supports jumping to definitions of functions and macros loaded in the session.
- **Completion:** Snail is also integrated with the built-in Emacs [completion-at-point](https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html) facility. Provided it is configured with the `company-capf` backend, [company-mode](http://company-mode.github.io/) completion will also work (this should be the case by default).
- **Parser:** Snail uses [CSTParser](https://github.com/julia-vscode/CSTParser.jl), a full-featured Julia parser, to infer the structure of source files and to enable features which require an understanding of code context, especially the module in which a particular piece of code lives. This enables awareness of the current module for completion and cross-referencing purposes.


## Demo

https://user-images.githubusercontent.com/10327/128589405-7368bb50-0ef3-4003-b5d8-2be1da102576.mp4


## Installation

Julia versions >1.0 should all work with Snail.

Snail’s Julia-side dependencies will automatically be installed when it starts, and will stay out of your way using Julia’s [`LOAD_PATH` mechanism](https://docs.julialang.org/en/v1/base/constants/#Base.LOAD_PATH).

On the Emacs side:

1. Make sure you have Emacs 26.2 or later, compiled with module support (`--with-modules`). Check the value of `module-file-suffix`: it should be non-nil. (This is currently a default compile-time option Emacs distributed with [Homebrew](https://formulae.brew.sh/formula/emacs).)
2. Install [libvterm](https://github.com/neovim/libvterm). It is available in [Homebrew](https://formulae.brew.sh/formula/libvterm) and [Ubuntu 19.10](https://packages.ubuntu.com/eoan/libvterm-dev), and in source form on other systems.
3. Install [emacs-libvterm](https://github.com/akermu/emacs-libvterm) using your Emacs package manager. It is available from [MELPA](https://melpa.org/#/vterm) as `vterm`, so use something like `(package-install 'vterm)` or `(use-package vterm :ensure t)`. **It is important to do this step separately from the `julia-snail` installation, as you may run into problems with the Emacs package manager and byte-compiler!**
4. Verify that `vterm` works by running `M-x vterm` to start a shell. It should display a nice terminal buffer. You may find it useful to customize and configure `vterm`.
5. Install `julia-snail` using your Emacs package manager (see below for a sample `use-package` invocation). It is available on [MELPA](https://melpa.org/#/julia-snail) and [MELPA Stable](https://stable.melpa.org/#/julia-snail).

Optionally, install [markdown-mode](https://github.com/jrblevin/markdown-mode) to improve documentation buffer display.

Because Julia supports Unicode identifiers and uses them for mathematical symbols, it is also a good idea to double-check that your Emacs is cleanly set up to handle Unicode. The article [Working with Coding Systems and Unicode in Emacs](https://masteringemacs.org/article/working-coding-systems-unicode-emacs) explains the settings (though Emacs 27 seems to do the right thing by default).


## Configuration

### `use-package` setup

**Make sure to install vterm first!** (See the [Installation](#installation) section.) This (unfortunately) means that the order of `use-package` invocations below matters:

```elisp
(use-package vterm
  :ensure t)
;; Now run `M-x vterm` and make sure it works!

(use-package julia-snail
  :ensure t
  :hook (julia-mode . julia-snail-mode))
```


### Manual setup

Install dependencies as noted in the `Package-Requires` line of `julia-snail.el`. Then make sure `vterm` works, as described in the [Installation](#installation) section.

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

It is likely that most users will want the default REPL pop-up behavior to split the window vertically, but the default `split-window-sensibly` implementation only splits that way if `split-height-threshold` is smaller than the current window height. `split-height-threshold` defaults to 80 (lines), and relatively few windows will be that tall. Therefore, consider adding something like the following to your configuration:

```elisp
(customize-set-variable 'split-height-threshold 15)
```


### Other customizations

- `julia-snail-use-emoji-mode-lighter` (default `t`) — attempt to use a 🐌 emoji in the Emacs modeline lighter if the display supports it. Set to `nil` to use the ASCII string `"Snail"` instead (a `:diminish` override in `use-package` should also work).


## Usage

The following describes basic Snail functionality. Refer to the [Snail project wiki](https://github.com/gcv/julia-snail/wiki) for additional information, including a [Tips and Tricks](https://github.com/gcv/julia-snail/wiki/Tips-and-Tricks) section.


### Basics

Once Snail is properly installed, open a Julia source file. If `julia-mode-hook` has been correctly configured, `julia-snail-mode` should be enabled in the buffer (look for the Snail lighter in the modeline).

Start a Julia REPL using `M-x julia-snail` or `C-c C-z`. This will load all the Julia-side supporting code Snail requires, and start a server. The server runs on a TCP port (10011 by default) on localhost. You will see `JuliaSnail.start(<port>)` execute on the REPL.

The REPL buffer uses `libvterm` mode, and `libvterm` configuration and key bindings will affect it.

If the Julia program uses Pkg, then run `M-x julia-snail-package-activate` or `C-c C-a` to enable it. (Doing this using REPL commands like `]` also works as normal.)

Load the current Julia source file using `M-x julia-snail-send-buffer-file` or `C-c C-k`. Notice that the REPL does not show an `include()` call, because the command executed across the Snail network connection. Among other advantages, this minimizes REPL history clutter.

Users of Revise should load it normally into the session, and do not need to use `julia-snail-send-buffer-file`.

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

In addition, most `xref` commands are available (except `xref-find-references`). `xref-find-definitions`, by default bound to `M-.`, does a decent job of jumping to function and macro definitions. Cross-reference commands are current-module aware where it makes sense.

Completion also works. Emacs built-in completion features, as well as `company-complete`, will do a reasonable job of finding the right completions in the context of the current module (though will not pick up local variables). Completion is current-module aware.


### Multiple Julia versions

The `julia-snail-executable` variable can be set at the file level or at the directory level and point to different versions of Julia for different projects. It should be a string referencing the executable binary path.

**NB:** On a Mac, the Julia binary is typically `Contents/Resources/julia/bin/julia` inside the distribution app bundle. You must either make sure `julia-snail-executable` is set to an absolute path, or configure your Emacs `exec-path` to correctly find the `julia` binary.


### Multiple REPLs

To use multiple REPLs, set the local variables `julia-snail-repl-buffer` and `julia-snail-port`. They must be distinct per-project. They can be set at the [file level](https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html), or at the [directory level](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html). The latter approach is recommended, using a `.dir-locals.el` file at the root of a project directory. (Emacs provides numerous interactive helper functions to help deal with file and directory variable scope: `add-dir-local-variable`, `delete-dir-local-variable`, `copy-dir-locals-to-file-locals`, `copy-dir-locals-to-file-locals-prop-line`, and `copy-file-locals-to-dir-locals`. Users of Projectile have additional tools at their disposal: `projectile-edit-dir-locals` and `projectile-skel-dir-locals`.)

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


### Remote REPLs

Snail can use a Julia REPL instance running on a remote host using SSH tunneling and Emacs [Tramp](https://www.gnu.org/software/tramp/), subject to the following conditions:

1. A full Julia environment must be installed on the remote host.
2. The code under development is likewise on the remote host. Emacs must open source files using Tramp.
3. SSH access to the remote host must be configured with no-password access. Several ways exist to configure SSH to do this, all beyond the scope of the Snail README (hints: either use an agent or the ControlMaster setting; do _not_ just copy a no-passphrase key to the remote host!).

If all these things are true, visit a remote Julia source file using Tramp, and start `julia-snail`. It should transparently start a fully-functional remote REPL.

Just as with local Julia sessions, Snail can be configured using a remote `.dir-locals.el` file or another method for setting variables. In particular, `julia-snail-executable` may need to be changed.

The SSH tunnel will, by default, open from `julia-snail-port` on the remote host to the same port on localhost. The remote host’s port can be changed by setting `julia-snail-remote-port`.

**NB:** To use `.dir-locals.el` over Tramp, you must set `enable-remote-dir-locals` to `t`!


#### `julia-snail-executable` and the remote shell path

**NB:** It is simpler to set `julia-snail-executable` in your project to the Julia binary’s absolute path than to wrangle your shell path. This section gives a bit of assistance if you disregard this advice.

A subtle problem may occur if `julia-snail-executable` is set to a value you expect to find on the remote host’s shell path. When Snail connects to the remote host using SSH, it will launch Julia in a _non-interactive_, _non-login_ shell. This means that, depending on (1) your remote shell, (2) how you set your path, and (3) which shell startup files you rely on, the path may not be what you have in your ordinary remote shell sessions.

- Zsh: `.zshenv` is always executed; `.zshrc` is only executed by interactive shells. _Make sure you set your path in `.zshenv`._
- Bash: `.bashrc` is only executed by non-interactive shells; `.bash_profile` is only executed by interactive shells. To run your setup regardless of shell type, put everything in `.bashrc` and source `.bashrc` from `.bash_profile.`


#### Remote environment setup

You may encounter a situation in which your remote host’s shell is configured to read startup files you do not control, and this setup does not occur when you attempt to launch a remote REPL. This may happen, e.g., in a computing cluster, where some startup files are required to correctly set up your environment (libraries, paths, and so forth) but are not being read by non-interactive non-login shells. In that case, it may be useful to force those extra scripts to load. Try adding the following code in your remote `.bashrc` or `.zshenv` scripts:

```shell
# bash (replace /etc/profile in both places with the file you need):
[[ ! $(shopt -q login_shell) && $- != *i* && -f /etc/profile ]] && . /etc/profile

# zsh (replace /etc/profile in both places with the file you need):
[[ ! -o login && ! -o interactive && -f /etc/profile ]] && . /etc/profile
```


### Docker container REPLs

Snail can use a Julia REPL instance running inside a Docker container. Like [SSH remote REPLs](#remote-repls), this uses [Tramp](https://www.gnu.org/software/tramp/). To make this work:

1. The [docker-tramp](https://github.com/emacs-pe/docker-tramp.el) package must be installed in the local Emacs instance. (Snail does not automatically install this dependency because the container feature is optional.)
2. A full Julia environment is available in-container.
3. The code under development is available in-container (using volumes or some other mechanism).
4. Ports must be appropriately mapped in-container.

A sample container invocation should help understand the requirements:
```shell
docker run --name julia-1 --rm -it -p 127.0.0.1:10011:10011 -v "${HOME}/work:/work" julia bash
```

Visit an in-container Julia source file using Tramp, and start `julia-snail`. It should transparently start an in-container REPL.

Snail configuration, in particular port mapping, works the same as for remote SSH REPLs.


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


### Multimedia and plotting

Snail supports making diagrams by plugging into Julia’s [multimedia I/O](https://docs.julialang.org/en/v1/base/io-network/#Multimedia-I/O) system. Any plot back-end which generates SVG or PNG output can display in an Emacs buffer, provided the Emacs instance itself supports images.

To enable Emacs-Julia multimedia integration, either (1) set local variable `julia-snail-multimedia-enable` to `t`, preferably in `.dir-locals.el`, or (2) after the Julia REPL connects to Emacs, call the function `julia-snail-multimedia-toggle-display-in-emacs`.

With Emacs multimedia display turned on, plotting commands in packages like Plots and Gadfly will display an Emacs buffer.

The following variables control multimedia integration. It is best to set these in the project’s `.dir-locals.el`.

- `julia-snail-multimedia-enable`: When set before starting a REPL, this turns on Emacs multimedia integration.
- `julia-snail-multimedia-buffer-autoswitch`: Controls whether Emacs should automatically switch to the image buffer after a plotting command, or if it should only display it. Defaults to `nil` (off).
- `julia-snail-multimedia-buffer-style`: Controls how the multimedia display buffer works. When `:single-reuse` (default), it uses one buffer, and overwrites it with new images as they come in from Julia. When set to `:single-new`, Snail will open a new buffer for each plot. When set to `:multi`, Snail uses a single buffer but appends new images to it rather than overwriting them. Note that `:multi` inserts image objects, but does not enable `image-mode` in the buffer, thus limiting zoom capabilities.

As a simple example, activate Emacs plotting and try this code:

```julia
Pkg.add("Gadfly")
import Gadfly
Gadfly.plot(sin, 0, 2π)
```


### `code-cells` integration (notebook mode)

Snail can be used with the [`code-cells`](https://github.com/astoff/code-cells.el) package for (Jupyter) notebook-style work. Sample configuration follows:

```elisp
(use-package code-cells
  :hook (julia-mode . code-cells-mode)
  :config
  (add-to-list 'code-cells-eval-region-commands '(julia-snail-mode . julia-snail-send-code-cell)))
```


## Extensions

Snail supports opt-in extensions. The `julia-snail-extensions` variable controls which extensions load in a given Snail session. As most other Snail configuration variables, it is best set at a project level using `.dir-locals.el`:

```elisp
((julia-mode . ((julia-snail-extensions . (repl-history formatter)))))
```

Extensions will be enabled at Snail startup, and will install their own Julia-side dependencies as needed into their own individual Pkg environments.

An annoyance: because Snail extensions do not use Elisp autoloading, customizing their keybindings must be performed in an explicit `with-eval-after-load` form. This cannot (currently) be done in a `use-package` `:bind` directive. To change extension keybindings, use the following pattern (which can be placed in a `use-package` `:config` directive):

```elisp
(with-eval-after-load 'julia-snail/repl-history
  (define-key julia-snail/repl-history-mode-map (kbd "H-y") #'julia-snail/repl-history-search-and-yank))
```


### REPL history

This extension provides access Julia REPL history from `julia-snail-mode` buffers using the following commands (the letters in the key sequences stand for **j**ulia **r**epl **h**istory):

- `julia-snail/repl-history-search-and-yank` provides a search interface (<kbd>C-c j r h C-s</kbd>)
- `julia-snail/repl-history-yank` yanks recent history entries (1 by default) (<kbd>C-j r h C-y</kbd>)
- `julia-snail/repl-history-buffer` opens a buffer with the history (<kbd>C-c j r h C-o</kbd>)


### Formatter

This extension uses [JuliaFormatter.jl](https://github.com/domluna/JuliaFormatter.jl) to modify source buffer text (the letters in the key sequences stand for **j**ulia **f**ormatter):

- `julia-snail/formatter-format-region` modifies the current region (<kbd>C-c j f r</kbd>)
- `julia-snail/formatter-format-buffer` modifies the entire current buffer (<kbd>C-c j f b</kbd>)


## Future improvements

-  The `libvterm` dependency forces the use of recent Emacs releases, forces Emacs to be build with module support, complicates support for Windows, and is generally quite gnarly. It would be much better to re-implement the REPL in Elisp.
- Completion does not pick up local variables.
- A real eldoc implementation would be great, but difficult to do with Julia’s generic functions.
- A debugger would be great.
- A real test suite which fully drives both Julia and Emacs and runs in a CI environment (like GitHub Actions) wouldn’t hurt, either.
