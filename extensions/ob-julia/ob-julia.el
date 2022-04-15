;;; ob-julia.el --- Julia Snail -*- lexical-binding: t -*-

;;; --- requirements

(require 'julia-snail)

(defun julia-snail/ob-julia-evaluate (body src-file out-file)
  (let* ((filename (julia-snail--efn (buffer-file-name (buffer-base-buffer))))
         (module :Main)
         (line-num 0)
		 (text (format "JuliaSnail.Extensions.ObJulia.babel_run_and_store(\"%s\", \"%s\")" src-file out-file)))
    ;; (julia-snail--send-to-repl text)
	(julia-snail--send-to-server :Main text)))

(defun org-babel-execute:julia (body params)
  (let ((src-file (org-babel-temp-file "julia-src-"))
		(out-file (org-babel-temp-file "julia-out-")))
	(with-temp-file src-file (insert body))
	(julia-snail/ob-julia-evaluate body src-file out-file)
	(let ((c 0))
      (while (and (< c 100) (= 0 (file-attribute-size (file-attributes out-file))))
		(sit-for 0.1)
		(setq c (1+ c))))
    (with-temp-buffer
      (insert-file-contents out-file)
      (let ((bs (buffer-string)))
		(if (catch 'loop
			  (dolist (line (split-string bs "\n"))
				(if (> (length line) 12000)
					(throw 'loop t))))
			"Output suppressed (line too long)"
		  bs)))))

(defun julia-snail/ob-julia-init (repl-buf)
  (julia-snail--send-to-server
    '("JuliaSnail" "Extensions")
    "load([\"ob-julia\" \"ObJulia.jl\"])"
    :repl-buf repl-buf
    :async nil))


(provide 'julia-snail/ob-julia)

;;; repl-history.el ends here
