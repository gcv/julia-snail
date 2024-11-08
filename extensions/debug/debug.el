;;; debug.el --- Julia Snail -*- lexical-binding: t -*-

(defun julia-snail/debug-init (repl-buf)
  (julia-snail--send-to-server
    '("JuliaSnail" "Extensions")
    "load([\"debug\" \"Debug.jl\"]); Debug.init()"
    :repl-buf repl-buf
    :async nil
    :async-poll-maximum 120000))

(provide 'julia-snail/debug)
