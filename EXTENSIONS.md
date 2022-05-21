# Extensions

Snail makes it fairly easy to write code that makes Emacs and Julia talk to each other. Self-contained features that integrate Julia libraries with Emacs should be written as Snail extensions. The following guidelines should help understand how to write an extension, and the existing [`repl-history`](extensions/repl-history) and [`formatter`](extensions/formatter) are good examples.


## General

- Put Snail extensions into subdirectories of `extensions/`.
- Extensions must provide strictly opt-in functionality.


## Elisp

- Extension names should follow Lisp naming conventions, i.e., use [kebab-case](https://en.wikipedia.org/wiki/Letter_case#Kebab_case).
- Extensions must follow Elisp library conventions, in particular with regard to `provide` forms. They should use lexical scope.
- Symbol names in extensions must start with the string `julia-snail/` followed by the extension name. This is used a namespace prefix.
- `julia-snail/<extension>-init`, when provided, is run when an extension loads. This is the right place to load any supporting Julia-side code.
- Extensions wishing to provide key bindings should provide a minor mode named `julia-snail/<extension>-mode` and `julia-snail/<extension>-mode-map`. This mode will be activated along with `julia-snail-mode` in source buffers.


## Julia

- Extensions with external Julia library dependencies must provide their own `Project.toml` files. This is done to keep all module dependencies isolated and keep all extensions strictly opt-in. A user who does not wish to use a particular extension must never be forced to install its Julia-side dependencies or spend time waiting for time-to-first-plot related to that unused extension.
- Use the `JuliaSnail.@with_pkg_env` macro to force extension dependencies to load. The first argument should be `(@__DIR__)` (the parentheses around it matter), and the second a block which contains dependency `import` statements. Dependencies will install if needed, and will be attached to the extension's Pkg environment.
- Extensions will be placed in the `JuliaSnail.Extensions` module. They should be inside their own individual modules, e.g., `JuliaSnail.Extensions.REPLHistory`.
