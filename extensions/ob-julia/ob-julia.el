;;; ob-julia.el --- Julia Snail -*- lexical-binding: t -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A Julia Snail extension to work with Org Babel

;;; Code:


;;; --- requirements

(require 'julia-snail)
(require 'ob-core)


;;; --- customizations

(defvar org-babel-default-header-args:julia '((:wrap) (:module . "Main")))

(defcustom julia-snail/ob-julia-use-error-pane t
  "If true, use julia-snail's popup error pane. Otherwise, display errors inline"
  :tag "Control the behaviour of errors thrown during Julia evaulation"
  :group 'julia-snail
  :safe 'booleanp
  :type 'boolean)


;;; --- implementation

(defun julia-snail/ob-julia-evaluate (module _body src-file out-file)
  (let* (;;(filename (julia-snail--efn (buffer-file-name (buffer-base-buffer)))) ; commented out to make byte-compiler happy
         ;;(line-num 0)                                                          ; commented out to make byte-compiler happy
	 (text (format "JuliaSnail.Extensions.ObJulia.babel_run_and_store(%s, \"%s\", \"%s\", %s)"
		       module
		       src-file
		       out-file
		       (if julia-snail/ob-julia-use-error-pane "true" "false"))))
    ;; This code was meant to startup julia-snail in the org buffer if it's not active, but caused an error
    ;; in org-mode on showing the first result of evalutation. Not sure why.
    ;; (unless (get-buffer julia-snail-repl-buffer)
    ;;   (progn
    ;; 	(julia-snail) t))
    (julia-snail--send-to-server :Main text)))

;; This function was adapted from ob-julia-vterm by Shigeaki Nishina (GPL-v3)
;; https://github.com/shg/ob-julia-vterm.el as of April 14, 2022
(defun org-babel-execute:julia (body params)
  (let ((src-file (concat (org-babel-temp-file "julia-src-") ".jl"))
	(out-file (org-babel-temp-file "julia-out-"))
	(module (let ((maybe-module (cdr (assq :module params))))
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

;; Deal with colour ANSI escape colour codes
;; from https://emacs.stackexchange.com/a/63562/19896
(defun ek/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))
(add-hook 'org-babel-after-execute-hook 'ek/babel-ansi)


(define-minor-mode julia-snail-org-interaction-mode
  "Minor mode for interacting with julia-snail through an org-mode buffer. So far this only has implemented completion inside `julia` blocks."
  :group 'ob-julia
  :init-value nil
  (if julia-snail-org-interaction-mode (add-hook 'completion-at-point-functions 'ob-julia-completion-at-point nil t)
	(remove-hook 'completion-at-point-functions 'ob-julia-completion-at-point t)))


(add-hook 'org-mode-hook 'julia-snail-org-interaction-mode)

(defun ob-julia-completion-at-point ()
  (let ((info (org-babel-get-src-block-info)))
	(when (and info (string-equal (nth 0 info) "julia"))
	  (let ((identifier (julia-snail--identifier-at-point))
			(bounds (julia-snail--identifier-at-point-bounds))
			(split-on "\\.")
			(prefix "")
            (module (split-string (or (cdr (assq :module (nth 2 info))) "Main") "\\."))
			start)
		(when bounds
		  ;; If identifier starts with a backslash we need to add an extra "\\" to
		  ;; make sure that the string which arrives to the completion provider on the server starts with "\\".
		  (when (s-equals-p (substring identifier 0 1) "\\")
			(setq prefix "\\"))
		  ;; check if identifier at point is inside a string and attach the opening quotes so
		  ;; we get path completion.
		  (when-let (prev (char-before (car bounds)))
			(when (char-equal prev ?\")
			  (setq identifier (concat "\\\"" identifier))
			  ;; TODO: add support for Windows paths (splitting on "\\" when appropriate)
			  (setq split-on "/")))
		  ;; If identifier is not a string, we split on "." so that completions of
		  ;; the form Module.f -> Module.func work (since
		  ;; `julia-snail--repl-completions' will return only "func" in this case)
		  (setq start (- (cdr bounds) (length (car (last (s-split split-on identifier))))))
		  (list start
				(cdr bounds)
				(completion-table-dynamic
				 (lambda (_)
				   (ob-julia-repl-completions (concat prefix identifier) module)
				   ))
				:exclusive 'no))))))

(defun ob-julia-repl-completions (identifier module)
  (let* ((res (julia-snail--send-to-server
                :Main
                (format "try; JuliaSnail.replcompletion(\"%1$s\", %2$s); catch; JuliaSnail.replcompletion(\"%1$s\", Main); end"
                        identifier
                        (s-join "." module))
                :async nil)))
    (if (eq :nothing res)
        (list)
      res)))


;;; --- initialiation function

(defun julia-snail/ob-julia-init (repl-buf)
  (julia-snail--send-to-server
    '("JuliaSnail" "Extensions")
    "load([\"ob-julia\" \"ObJulia.jl\"])"
    :repl-buf repl-buf
    :async nil))


;;; --- done

(provide 'julia-snail/ob-julia)


;;; ob-julia.el ends here
