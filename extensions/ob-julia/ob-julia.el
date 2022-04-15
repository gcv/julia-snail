;;; ob-julia.el --- Julia Snail -*- lexical-binding: t -*-

;;; --- requirements

(require 'julia-snail)

(defvar org-babel-default-header-args:julia '((:wrap)
											  (:module . "Main")))

(defun julia-snail/ob-julia-evaluate (module body src-file out-file)
  (let* ((filename (julia-snail--efn (buffer-file-name (buffer-base-buffer))))
         (line-num 0)
		 (text (format "JuliaSnail.Extensions.ObJulia.babel_run_and_store(%s, \"%s\", \"%s\")" module src-file out-file)))
    ;; (julia-snail--send-to-repl text)
	;; (unless (get-buffer julia-snail-repl-buffer)
	;;   (progn
	;; 	(julia-snail) t))
	(julia-snail--send-to-server :Main text)))

(defun org-babel-execute:julia (body params)
  (let ((src-file (org-babel-temp-file "julia-src-"))
		(out-file (org-babel-temp-file "julia-out-"))
		(module (let (maybe-module (cdr (assq :module params)))
					(if maybe-module maybe-module "Main"))))
	(with-temp-file src-file (insert body))
	(julia-snail/ob-julia-evaluate module body src-file out-file)
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
;;; ob-julia.el ends here

(if nil "bye" "lie")
