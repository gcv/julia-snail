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
(require 'org-element)


;;; --- customizations

(defvar org-babel-default-header-args:julia '((:wrap) (:module . "Main")))

(defcustom julia-snail/ob-julia-use-error-pane t
  "If true, use julia-snail's popup error pane. Otherwise, display errors inline"
  :tag "Control the behaviour of errors thrown during Julia evaulation"
  :group 'julia-snail
  :safe 'booleanp
  :type 'boolean)

(defcustom julia-snail/ob-julia-mirror-output-in-repl t
  "If true, all output from code evaluated in ob-julia will also be shown in the julia REPL.
Note that due to problem with async evaluation, trying to use emacs while julia code is running
will cause your program's output to not be shown in org-mode, so this is currently a bad idea
to disable."
  :tag "Control the display of code evaluation in the Julia REPL"
  :group 'julia-snail
  :safe 'booleanp
  :type 'boolean)


;;; --- implementation

(defun julia-snail/ob-julia-evaluate (module _body src-file out-file)
  (let* (;;(filename (julia-snail--efn (buffer-file-name (buffer-base-buffer)))) ; commented out to make byte-compiler happy
         ;;(line-num 0)                                                          ; commented out to make byte-compiler happy
         (text (format "JuliaSnail.Extensions.ObJulia.babel_run_and_store(%s, \"%s\", \"%s\", %s, %s)"
                       module
                       src-file
                       out-file
                       (if julia-snail/ob-julia-use-error-pane "true" "false")
                       (if julia-snail/ob-julia-mirror-output-in-repl "true" "false"))))
    ;; This code was meant to startup julia-snail in the org buffer if it's not active, but caused an error
    ;; in org-mode on showing the first result of evalutation. Not sure why.
    ;; (unless (get-buffer julia-snail-repl-buffer)
    ;;   (progn
    ;;  (julia-snail) t))
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
(defun julia-snail/ob-julia-abel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))
(add-hook 'org-babel-after-execute-hook #'julia-snail/ob-julia-abel-ansi)

(defun julia-snail/ob-julia--module-for-src-block ()
  (let ((info (org-babel-get-src-block-info)))
    (when (and info (string-equal (nth 0 info) "julia"))
      (split-string (or (cdr (assq :module (nth 2 info))) "Main") "\\."))))

(defun julia-snail/ob-julia--module-at-point ()
  (let* ((src-module (julia-snail/ob-julia--module-for-src-block))
         (context (org-element-context (org-element-at-point)))
         (beg (org-element-property :begin context))
         (end (org-element-property :end context))
         (contents (buffer-substring beg end))
         (pt (- (point) beg))
         (inner-module (with-temp-buffer
                         (insert contents)
                         (julia-snail--cst-module-at (current-buffer) pt))))
    (if inner-module
        (append src-module inner-module)
      src-module)))

(defun julia-snail/ob-julia-completion-at-point ()
  "Check if point is inside an org julia SRC block, and if so, use julia-snail repl completions"
  (let ((info (org-babel-get-src-block-info)))
    (when (and info (string-equal (nth 0 info) "julia"))
      (julia-snail-repl-completion-at-point #'julia-snail/ob-julia--module-at-point))))

(define-minor-mode julia-snail/ob-julia-interaction-mode
  "Minor mode for interacting with julia-snail through an org-mode buffer. So far this only has implemented completion inside `julia` blocks."
  :group 'julia-snail
  :init-value nil
  (cond
   (julia-snail/ob-julia-interaction-mode
    (add-hook 'completion-at-point-functions 'julia-snail/ob-julia-completion-at-point nil t))
   (t
    (remove-hook 'after-revert-hook 'julia-snail-interaction-mode t))))


;;; --- initialiation function

(defvar julia-snail/ob-julia--has-initialized nil)

(defun julia-snail/ob-julia-init (repl-buf)
  (julia-snail--send-to-server
    '("JuliaSnail" "Extensions")
    "load([\"ob-julia\" \"ObJulia.jl\"])"
    :repl-buf repl-buf
    :async nil)
  (add-hook 'org-mode-hook #'julia-snail/ob-julia-interaction-mode)
  (unless julia-snail/ob-julia--has-initialized
    (mapc (lambda (buf) (with-current-buffer buf
                          (if (string-equal major-mode "org-mode")
                              (julia-snail/ob-julia-interaction-mode))))
          (buffer-list))
    (setf julia-snail/ob-julia--has-initialized t)))


;;; --- done

(provide 'julia-snail/ob-julia)


;;; ob-julia.el ends here
