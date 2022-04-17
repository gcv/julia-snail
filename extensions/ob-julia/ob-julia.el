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


;;; --- customizations

(defvar org-babel-default-header-args:julia '((:wrap) (:module . "Main")))

(defcustom julia-snail/ob-julia-use-error-pane t
  "If true, use julia-snail's popup error pane. Otherwise, display errors inline"
  :tag "Control the behaviour of errors thrown during Julia evaulation"
  :group 'julia-snail
  :safe 'booleanp
  :type 'boolean)


;;; --- implementation

(defun julia-snail/ob-julia-evaluate (module body src-file out-file)
  (let* ((filename (julia-snail--efn (buffer-file-name (buffer-base-buffer))))
         (line-num 0)
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
